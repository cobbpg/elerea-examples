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

infixl 7 ^*.
infixl 7 .*^
infixl 7 ^/.
infixl 7 `dot`
infixl 7 `cross`
infixl 6 ^+^
infixl 6 ^-^

data Vec = V { getX :: {-# UNPACK #-} !GLfloat, getY :: {-# UNPACK #-} !GLfloat }

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
  mul :: v -> v -> v

instance Vector2D Vec GLfloat where
  V x1 y1 ^+^ V x2 y2 = V (x1+x2) (y1+y2)
  V x1 y1 ^-^ V x2 y2 = V (x1-x2) (y1-y2)
  V x y ^*. t = V (x*t) (y*t)
  t .*^ V x y = V (x*t) (y*t)
  V x y ^/. t = V (x/t) (y/t)
  vnull = V 0 0
  V x1 y1 `dot` V x2 y2 = x1*x2+y1*y2
  V x1 y1 `cross` V x2 y2 = x1*y2-x2*y1
  vlen (V x y) = sqrt (x*x+y*y)
  V x1 y1 `mul` V x2 y2 = V (x1*x2) (y1*y2)

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
  mul = liftA2 mul

frameThickness = 0.05

ballSize = 0.05

ballFade = 2

data Ball = Ball { ballPos :: Vec
                 , ballCol :: Maybe GLfloat
                 , ballDrag :: Bool
                 }

balls mousePos mousePress = mdo
  leftPress <- memo $ fst <$> mousePress

  ballList <- collectE newBallE
  ballData <- memo $ sequence =<< ballList
  newBallCond <- edge $ leftPress &&@ (all (not.ballDrag) <$> ballData)
  newBallE <- generator $ newBall <$> mousePos <*> newBallCond
  let newBall pos cond = if cond
                         then Just <$> ball pos mousePos leftPress
                         else return Nothing

  return $ render <$> ballData

ball initPos mousePos mouseDown = mdo
  mouseClick <- edge mouseDown
  let dragBegin = mouseClick &&@ (vlen (pos^-^mousePos) <@ ballSize/2)
  dragEnd <- edge (not <$> mouseDown)
  drag <- False --> leftE (ifE dragBegin (pure True)) (ifE dragEnd (pure False))
  dragVel <- derivTV 0.05 mousePos
  
  let collHorz (V vx _) (V px _) = collFrame vx px
      collVert (V _ vy) (V _ py) = collFrame vy py
      collFrame v p = abs (p-0.5) > 0.5-(frameThickness+ballSize/2) && (p-0.5)*v > 0
  pos <- integralV initPos vel
  vel <- vnull --> velE
  velE <- memo $ mergeE [ifE drag dragVel,
                         ifE (collHorz <$> vel <*> pos) (mul (V (-1) 1) <$> vel),
                         ifE (collVert <$> vel <*> pos) (mul (V 1 (-1)) <$> vel)]

  colour <- ballColour (isJust <$> velE)

  return $ Ball <$> pos <*> colour <*> drag

ballColour hit = transfer Nothing update hit
  where update dt True  _    = Just 1
        update dt False prev = do t <- prev
                                  guard (t > 0)
                                  return (t-dt*ballFade)

-- Some generally useful combinators

integral v0 s = transfer v0 (\dt v v0 -> v0+v*dt) s

integralV v0 s = transfer v0 (\dt v v0 -> v0^+^(v^*.dt)) s

deriv s = do
  sig <- transfer (0,0,1) (\dt v (v0,_,_) -> (v,v0,dt)) s
  initSignal 0 (d <$> sig)
  where d (x',x,dt) = (x'-x)/dt

derivV s = do
  sig <- transfer (vnull,vnull,1) (\dt v (v0,_,_) -> (v,v0,dt)) s
  initSignal vnull (d <$> sig)
  where d (x',x,dt) = (x'^-^x)^/.dt

derivT wt s = do
  sig <- transfer (0,(0,0)) d s
  return (fst <$> sig)
  where d dt v (x,(v0,t)) = if t' > wt then ((v-v0)/t',(v,0)) else (x,(v0,t'))
          where t' = dt+t

derivTV wt s = do
  sig <- transfer (vnull,(vnull,0)) d s
  return (fst <$> sig)
  where d dt v (x,(v0,t)) = if t' > wt then ((v^-^v0)^/.t',(v,0)) else (x,(v0,t'))
          where t' = dt+t

movingAvg n s = do
  sig <- scanM n (delay 0) s
  return $ ((/n).sum) <$> sequence sig

movingAvgV n s = do
  sig <- scanM n (delay vnull) s
  return $ ((^/.n).foldl1' (^+^)) <$> sequence sig

scanM 0 _ _ = return []
scanM n f m = do
  x <- f m
  xs <- scanM (n-1) f x
  return (x:xs)

-- Thanks to the signal monad instance these can be more efficient
-- than the purely applicative versions!

ifS c s1 s2 = c >>= \b -> if b then s1 else s2

ifE c s = c >>= \b -> if b then Just <$> s else return Nothing

leftE e1 e2 = e1 >>= \mx -> case mx of
  Nothing -> e2
  jx      -> return jx

rightE e1 e2 = e2 >>= \mx -> case mx of
  Nothing -> e1
  jx      -> return jx

mergeE []     = return Nothing
mergeE (e:es) = e >>= \mx -> case mx of
  Nothing -> mergeE es
  jx      -> return jx

collectE e = mdo
  sig <- delay [] (((++).maybeToList) <$> e <*> sig)
  return sig

initSignal x s = do
  c <- stateful True (const (const False))
  return $ ifS c (pure x) s

-- OpenGL bits and general plumbing

driveNetwork network driver = do
  dt <- driver
  case dt of
    Just dt -> do join $ network dt
                  driveNetwork network driver
    Nothing -> return ()

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
  forM_ bs $ \b -> do
    case ballCol b of
      Nothing -> color $ Color4 0 0 0.7 (1 :: GLfloat)
      Just t -> color $ Color4 (0.9*t) (0.9*t) (0.7*(1-t)) (1 :: GLfloat)

    let V x y = ballPos b
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
