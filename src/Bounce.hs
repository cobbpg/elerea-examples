{-# LANGUAGE RecursiveDo, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, NoMonomorphismRestriction #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import FRP.Elerea.Experimental
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL

data Vec = V { getX :: {-# UNPACK #-} !GLfloat, getY :: {-# UNPACK #-} !GLfloat }

infixl 7 ^*.
infixl 7 .*^
infixl 7 ^/.
infixl 7 `dot`
infixl 7 `cross`
infixl 6 ^+^
infixl 6 ^-^

class Vector2D v c | v -> c where
  (^+^) :: v -> v -> v
  (^-^) :: v -> v -> v
  (^*.) :: v -> c -> v
  (.*^) :: c -> v -> v
  (^/.) :: v -> c -> v
  vnull :: v
  dot :: v -> v -> c
  cross :: v -> v -> c
  vlen :: v -> c

instance Vector2D Vec GLfloat where
  V x1 y1 ^+^ V x2 y2 = V (x1+x2) (y1+y2)
  V x1 y1 ^-^ V x2 y2 = V (x1-x2) (y1-y2)
  V x y ^*. t = V (x*t) (y*t)
  t .*^ V x y = V (x*t) (y*t)
  V x y ^/. t = V (x/t) (y/t)
  vnull = V 0 0
  V x1 y1 `dot` V x2 y2 = x1*y1+x2*y2
  V x1 y1 `cross` V x2 y2 = x1*y2-x2*y1
  vlen (V x y) = sqrt (x*x+y*y)

instance Vector2D (Signal p Vec) (Signal p GLfloat) where
  (^+^) = liftA2 (^+^)
  (^-^) = liftA2 (^-^)
  (^*.) = liftA2 (^*.)
  (.*^) = liftA2 (.*^)
  (^/.) = liftA2 (^/.)
  vnull = pure vnull
  dot = liftA2 dot
  cross = liftA2 cross
  vlen = fmap vlen

frameThickness = 0.05

ballSize = 0.05

ballFade = 1

main = do
  initialize
  openWindow (Size 640 480) [DisplayRGBBits 8 8 8, DisplayAlphaBits 8, DisplayDepthBits 24] Window
  windowTitle $= "Elerea Bounce"

  (mousePosition,mousePositionSink) <- external vnull
  (mousePress,mousePressSink) <- external (False,False)

  closed <- newIORef False
  windowSizeCallback $= resizeGLScene
  windowCloseCallback $= writeIORef closed True
  initGL 640 480

  demo <- start $ balls mousePosition mousePress
  time $= 0
  driveNetwork demo (readInput mousePositionSink mousePressSink closed)

  closeWindow

mergeJust = undefined

drag = undefined

balls mousePos mousePress = mdo
{-
  let leftPress = fst <$> mousePress
      newBall b (V x y) (V w h) = if b then
                                    mdo vel <- vnull --> (mergeJust <$> drag <*> coll)
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
  
-}

  bc <- ballColour =<< edge (fst <$> mousePress)
  bp <- ballPosition (V 0.5 0.5) mousePos (fst <$> mousePress)
  let ballList = pure [(bp,bc)]

      flatBallList = do bs <- ballList
                        forM bs $ \(pos,col) -> liftA2 (,) pos col

  return $ render <$> flatBallList

ballPosition initPos mousePos mouseDown = mdo
  dragBegin <- edge (mouseDown &&@ (vlen (pos^-^mousePos) <@ 0.05/2))
  dragEnd <- edge (not <$> mouseDown)
  drag <- False --> fstmb <$> ifmb dragBegin (pure True) <*> ifmb dragEnd (pure False)
  pos <- integralVec initPos vel -- > ifmb drag mousePos
  --let mkPos isDrag newPos = if isDrag
  --                          then Just <$> integralVec newPos vel
  --                          else return Nothing
  --pos' <- generator (initSignal (integralVec initPos vel) (mkPos <$> drag <$> mousePos))
  --pos <- join <$> undefined --> pos'
  --vel' <- {- movingAverageVec 5 =<< -} derivativeVec mousePos
  vel' <- derivativeVecWindowed 0.05 mousePos
  vel <- vnull --> ifmb drag vel'
  --let vel = pure vnull
  return pos

ballColour hit = transfer Nothing update hit
  where update dt True  _    = Just 1
        update dt False prev = do t <- prev
                                  guard (t > 0)
                                  return (t-dt*ballFade)

render bs = do
  let drawRectangle x y sx sy = do
        renderPrimitive Quads $ do
          vertex $ Vertex3 (x)    (y)    (0 :: GLfloat)
          vertex $ Vertex3 (x+sx) (y)    (0 :: GLfloat)
          vertex $ Vertex3 (x+sx) (y+sy) (0 :: GLfloat)
          vertex $ Vertex3 (x)    (y+sy) (0 :: GLfloat)
      drawEllipse xc yc xs ys n = do
        renderPrimitive TriangleStrip $ forM_ [0..n] $ \i -> do
          let a = 2*pi*fromIntegral i/fromIntegral n
          vertex $ Vertex3 (xc+xs/2*sin a) (yc+ys/2*cos a) (0 :: GLfloat)
          vertex $ Vertex3 xc yc (0 :: GLfloat)

  clear [ColorBuffer]
  loadIdentity
  color $ Color4 0.7 0.7 0.7 (1 :: GLfloat)
  drawRectangle 0 0 1 frameThickness
  drawRectangle 0 (1-frameThickness) 1 frameThickness
  drawRectangle 0 0 frameThickness 1
  drawRectangle (1-frameThickness) 0 frameThickness 1
  forM_ bs $ \(V x y,col) -> do
    case col of
      Nothing -> color $ Color4 0 0 0.7 (1 :: GLfloat)
      Just t -> color $ Color4 0.9 0.9 0 (t*0.7+0.3 :: GLfloat)
    drawEllipse x y ballSize ballSize 20
 
  flush
  swapBuffers

readInput mousePos mouseBut closed = do
  t <- get time
  time $= 0
  
  Position x y <- get GLFW.mousePos
  Size w h <- get windowSize
  let x' = fromIntegral x
      y' = fromIntegral y
      w' = fromIntegral w
      h' = fromIntegral h
      mx = (x'-max 0 (w'-h')/2)/min w' h'
      my = (y'-max 0 (h'-w')/2)/min w' h'
  mousePos (V mx (1-my))
  
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

resizeGLScene size@(Size w h) = do
  let r = 2*fromIntegral w/fromIntegral h
      r' = 2*fromIntegral h/fromIntegral w

  viewport $= (Position 0 0,size)

  matrixMode $= Projection
  loadIdentity
  scale (min 2 r') (min 2 r) (1 :: GLfloat)
  translate $ Vector3 (-0.5) (-0.5) (0 :: GLfloat)
	
  matrixMode $= Modelview 0

driveNetwork network driver = do
  dt <- driver
  case dt of
    Just dt -> do join $ network dt
                  driveNetwork network driver
    Nothing -> return ()

integral v0 s = transfer v0 (\dt v v0 -> v0+v*dt) s

integralVec v0 s = transfer v0 (\dt v v0 -> v0^+^(v^*.dt)) s

derivative s = do
  sig <- transfer (0,0,1) (\dt v (v0,_,_) -> (v,v0,dt)) s
  initSignal 0 (d <$> sig)
  where d (x',x,dt) = (x'-x)/dt

derivativeVec s = do
  sig <- transfer (vnull,vnull,1) (\dt v (v0,_,_) -> (v,v0,dt)) s
  initSignal vnull (d <$> sig)
  where d (x',x,dt) = (x'^-^x)^/.dt

derivativeWindowed wt s = do
  sig <- transfer (0,(0,0)) d s
  return (fst <$> sig)
  where d dt v (x,(v0,t)) = if t' > wt then ((v-v0)/t',(v,0)) else (x,(v0,t'))
          where t' = dt+t

derivativeVecWindowed wt s = do
  sig <- transfer (vnull,(vnull,0)) d s
  return (fst <$> sig)
  where d dt v (x,(v0,t)) = if t' > wt then ((v^-^v0)^/.t',(v,0)) else (x,(v0,t'))
          where t' = dt+t

movingAverage n s = do
  sig <- scanM n (delay 0) s
  return $ ((/n).sum) <$> sequence sig

movingAverageVec n s = do
  sig <- scanM n (delay vnull) s
  return $ ((^/.n).foldl1' (^+^)) <$> sequence sig

scanM 0 _ _ = return []
scanM n f m = do
  x <- f m
  xs <- scanM (n-1) f x
  return (x:xs)

ifte = liftA3 (\c e1 e2 -> if c then e1 else e2)

ifmb = liftA2 (\c s -> if c then Just s else Nothing)

fstmb Nothing y = y
fstmb x       _ = x

initSignal x s = do
  isig <- stateful (Just x) (const (const Nothing))
  return $ fromJust <$> (fstmb <$> isig <*> (Just <$> s))

