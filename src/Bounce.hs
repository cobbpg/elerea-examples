{-# LANGUAGE RecursiveDo, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module Main where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.IORef
import Data.Maybe
import Data.Monoid
import FRP.Elerea.Experimental
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL

data Vec = V { getX :: !GLfloat, getY :: !GLfloat }

infixl 7 ^*.
infixl 7 .*^
infixl 7 `dot`
infixl 7 `cross`
infixl 6 ^+^
infixl 6 ^-^

class Vector2D v c | v -> c where
  (^+^) :: v -> v -> v
  (^-^) :: v -> v -> v
  (^*.) :: v -> c -> v
  (.*^) :: c -> v -> v
  vnull :: v
  dot :: v -> v -> c
  cross :: v -> v -> c

instance Vector2D Vec GLfloat where
  V x1 y1 ^+^ V x2 y2 = V (x1+x2) (y1+y2)
  V x1 y1 ^-^ V x2 y2 = V (x1-x2) (y1-y2)
  V x y ^*. t = V (x*t) (y*t)
  t .*^ V x y = V (x*t) (y*t)
  vnull = V 0 0
  V x1 y1 `dot` V x2 y2 = x1*y1+x2*y2
  V x1 y1 `cross` V x2 y2 = x1*y2-x2*y1

instance Vector2D (Signal p Vec) (Signal p GLfloat) where
  (^+^) = liftA2 (^+^)
  (^-^) = liftA2 (^-^)
  (^*.) = liftA2 (^*.)
  (.*^) = liftA2 (.*^)
  vnull = pure vnull
  dot = liftA2 dot
  cross = liftA2 cross

ballSize = 0.05

ballFade = 0.2

main = do
  initialize
  openWindow (Size 640 480) [DisplayRGBBits 8 8 8, DisplayAlphaBits 8, DisplayDepthBits 24] Window
  windowTitle $= "Elerea Bounce"

  (windowSize,windowSizeSink) <- external vnull
  (mousePosition,mousePositionSink) <- external vnull
  (mousePress,mousePressSink) <- external (False,False)

  closed <- newIORef False
  windowSizeCallback $= resizeGLScene windowSizeSink
  windowCloseCallback $= writeIORef closed True
  initGL 640 480

  demo <- start $ balls mousePosition mousePress windowSize
  driveNetwork demo (readInput mousePositionSink mousePressSink closed)

  closeWindow

balls mousePos mousePress windowSize = mdo
  let leftPress = fst <$> mousePress
      newBall b (V x y) (V w h) = if b then
                                    mdo vel <- storeJust vnull (mergeJust <$> drag <*> coll)
                                        pos <- integralVec initPos vel
                                        let coll = colliding pos
                                        clr <- ballColour (isJust <$> coll)
                                        return [(pos,vel,clr)]
                                  else return []
        where initPos = V (2*x/w-1-ballSize/2) (h/w-ballSize/2-2*y/w)
      colliding _ = pure Nothing

  rightClick <- edge (snd <$> mousePress)

  newBalls <- generator (newBall <$> rightClick <*> mousePos <*> windowSize)
  ballList <- liftA2 (++) newBalls <$> delay [] ballList

  leftDown <- edge leftPress
  leftUp <- edge (not <$> leftPress)
  
  let flatBallList = do bs <- ballList
                        forM bs $ \(pos,_,col) -> liftA2 (,) pos col

  return $ render <$> windowSize <*> flatBallList

ballColour hit = transfer Nothing update hit
  where update dt True  _    = Just 1
        update dt False prev = do t <- prev
                                  guard (t > 0)
                                  return (t-dt*ballFade)

render (V w h) bs = do
  let drawSquare x y s = do
        loadIdentity
        translate $ Vector3 (x/w*2-1) (h/w-y/w*2) 0
        renderPrimitive Quads $ do
          vertex $ Vertex3 (-s) (-s) (0 :: GLfloat)
          vertex $ Vertex3 ( s) (-s) (0 :: GLfloat)
          vertex $ Vertex3 ( s) ( s) (0 :: GLfloat)
          vertex $ Vertex3 (-s) ( s) (0 :: GLfloat)
      drawEllipse x y xs ys n = do
        let xc = x+xs/2
            yc = y+ys/2
        renderPrimitive TriangleStrip $ forM_ [0..n] $ \i -> do
          let a = 2*pi*fromIntegral i/fromIntegral n
          vertex $ Vertex3 (xc+xs/2*sin a) (yc+ys/2*cos a) (0 :: GLfloat)
          vertex $ Vertex3 xc yc (0 :: GLfloat)

  clear [ColorBuffer]
  color $ Color4 1 1 1 (0.6 :: GLfloat)
  forM_ bs $ \(V x y,_) -> drawEllipse x y ballSize ballSize 20
 
  flush
  swapBuffers

readInput mousePos mouseBut closed = do
  t <- get time
  time $= 0
  Position x y <- get GLFW.mousePos
  mousePos (V (fromIntegral x) (fromIntegral y))
  bl <- getMouseButton ButtonLeft
  br <- getMouseButton ButtonRight
  mouseBut (bl == Press, br == Press)
  k <- getKey ESC
  c <- readIORef closed
  return (if c || k == Press then Nothing else Just (realToFrac t))

initGL width height = do
  clearColor $= Color4 0 0 0 1
  blend $= Enabled
  blendFunc $= (SrcAlpha,OneMinusSrcAlpha)

resizeGLScene winSize size@(Size w h) = do
  winSize (V (fromIntegral w) (fromIntegral h))

  viewport $= (Position 0 0,size)

  matrixMode $= Projection
  loadIdentity
  scale 1 (fromIntegral w/fromIntegral h) (1 :: GLfloat)
	
  matrixMode $= Modelview 0

driveNetwork network driver = do
  dt <- driver
  case dt of
    Just dt -> do join $ network dt
                  driveNetwork network driver
    Nothing -> return ()

integral v0 s = transfer v0 (\dt v v0 -> v0+v*dt) s

integralVec v0 s = transfer v0 (\dt v v0 -> v0^+^(v^*.dt)) s
