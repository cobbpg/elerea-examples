> module Main where
> 
> import Control.Applicative
> import Control.Monad
> import Data.List
> import Data.Maybe
> import FRP.Elerea
> import Graphics.UI.GLFW as GLFW
> import Graphics.Rendering.OpenGL
> 
> import Common.Utils
> import Common.Vector
>
> main = do
>   initialize
>   openWindow (Size 640 480) [DisplayRGBBits 8 8 8, DisplayAlphaBits 8, DisplayDepthBits 24] Window
>   windowTitle $= "Elerea Breakout"
> 
>   (windowSize,windowSizeSink) <- external vnull
>   (mousePosition,mousePositionSink) <- external vnull
>   (mousePress,mousePressSink) <- external False
> 
>   windowSizeCallback $= resizeGLScene windowSizeSink
>   initGL 640 480
> 
>   driveNetwork (breakout mousePress mousePosition windowSize)
>                (readInput mousePositionSink mousePressSink)
> 
>   closeWindow
> 
> ballW = 0.04
> ballH = 0.04
> 
> brickW = 0.05
> brickH = 0.03
> brickPos0 = distributeBricks (-0.7) (-0.1) (0.7) (0.4) 18 10
>     where distributeBricks xmin ymin xmax ymax xn yn = [(xmin+xstep*x,ymin+ystep*y,Nothing) | x <- [0..xn-1], y <- [0..yn-1]]
>               where xstep = (xmax-xmin-xn*brickW)/(xn-1)+brickW
>                     ystep = (ymax-ymin-yn*brickH)/(yn-1)+brickH
> brickFade = 0.5

> playerY = -fieldH+0.01
> playerW = 0.2
> playerH = 0.03
> 
> fieldW = 0.8
> fieldH = 0.5
> 
> ballPos0 = V 0 (-0.2)
> ballVel0 = V (-0.4) (-0.35)
>
> vec :: GLfloat -> GLfloat -> Signal Vec
> vec x y = pure (V x y)
>
> breakout mousePress mousePos windowSize = renderLevel <$> playerX <*> ballPos <*> (getBricks <$> bricks)
>     where playerX = adjustPlayerPos <$> mousePos <*> windowSize
>           adjustPlayerPos (V x _) (V w _) = min (fieldW-playerW) $ max (-fieldW) $ 2*x/w-1-playerW/2
>
>           ballPos = integralVec (ballPos0) ballVel
>           ballVel = latcher (pure ballVel0) (ballCollHorz ||@ ballCollVert ||@ ballCollPlayer)
>                             (pure <$> (adjustVel <$> ballCollHorz <*> ballCollVert <*> ballCollPlayer <*> ballVel <*> ballNewVelX))
>           adjustVel ch cv cp (V bvx bvy) bvx' = V x y
>               where x = (if ch then -1 else 1)*(if cp then bvx'*4 else bvx)
>                     y = if cv || cp then -bvy else bvy
>           ballNewVelX = (getX <$> ballPos)-playerX-pure (playerW/2)
>
>           ballCollHorz = edge ((getHColl <$> bricks) ||@ (check <$> ballPos <*> ballVel))
>               where check (V bx _) (V bvx _) = (bx < -fieldW && bvx < 0) || (bx > fieldW-ballW && bvx > 0)
>           ballCollVert = edge ((getVColl <$> bricks) ||@ (check <$> ballPos <*> ballVel))
>               where check (V bx by) (V _ bvy) = by > fieldH-ballH && bvy > 0
>           ballCollPlayer = edge (check <$> ballPos <*> playerX)
>               where check (V bx by) px = doRectsIntersect bx by ballW ballH px playerY playerW playerH
>
>           bricks = transfer (False,False,brickPos0) updateBricks ballPos
>           getBricks (_,_,bs) = bs
>           getHColl (c,_,_) = c
>           getVColl (_,c,_) = c
>           updateBricks dt (V bx by) (_,_,bricksPrev) = (collHorz,collVert,bricksNext)
>               where bricksNext = catMaybes (map evolveBrick (map killBrick bricksDel++bricksRem))
>                     (bricksDel,bricksRem) = partition classifyBrick bricksPrev
>                     classifyBrick (x,y,a) = a == Nothing && doRectsIntersect bx by ballW ballH x y brickW brickH
>                     evolveBrick (x,y,Just a) = if a < fade then Nothing else Just (x,y,Just (a-fade))
>                         where fade = realToFrac dt*brickFade
>                     evolveBrick b = Just b
>                     killBrick (x,y,_) = (x,y,Just 1)
>                     collHorz = or collHBricks
>                     collVert = or (map not collHBricks)
>                     collHBricks = map isHorz bricksDel
>                     isHorz (x,y,_) = xDist/brickW > yDist/brickH
>                         where xDist = abs ((x+brickW/2)-(bx+ballW/2))
>                               yDist = abs ((y+brickH/2)-(by+ballH/2))
>
> doRectsIntersect x1 y1 sx1 sy1 x2 y2 sx2 sy2 = collIV x1 sx1 x2 sx2 && collIV y1 sy1 y2 sy2
>     where collIV p1 s1 p2 s2 = (p1 <= p2 && p2 <= p1+s1) || (p2 <= p1 && p1 <= p2+s2)
> 
> renderLevel playerX (V ballX ballY) bricks = do
>   let drawRect x y xs ys = do
>         loadIdentity
>         renderPrimitive Quads $ do
>           vertex $ Vertex3 (x)    (y)    (0 :: GLfloat)
>           vertex $ Vertex3 (x+xs) (y)    (0 :: GLfloat)
>           vertex $ Vertex3 (x+xs) (y+ys) (0 :: GLfloat)
>           vertex $ Vertex3 (x)    (y+ys) (0 :: GLfloat)
>   let drawEllipse x y xs ys n = do
>         let xc = x+xs/2
>             yc = y+ys/2
>         loadIdentity
>         renderPrimitive TriangleStrip $ forM_ [0..n] $ \i -> do
>           let a = 2*pi*fromIntegral i/fromIntegral n
>           vertex $ Vertex3 (xc+xs/2*sin a) (yc+ys/2*cos a) (0 :: GLfloat)
>           vertex $ Vertex3 xc yc (0 :: GLfloat)
> 
>   clear [ColorBuffer]
>
>   color $ Color4 0.2 0.2 0.2 (1 :: GLfloat)
>   drawRect (-fieldW) (-fieldH) (fieldW*2) (fieldH*2)
>
>   forM_ bricks $ \(x,y,a) -> do
>     case a of
>       Nothing -> color $ Color4 0.8 0.5 0.5 (0.6 :: GLfloat)
>       Just a' -> color $ Color4 0.9 0.9 0.2 (a' :: GLfloat)
>     drawRect x y brickW brickH
>
>   color $ Color4 1 1 1 (0.6 :: GLfloat)
>   drawEllipse ballX ballY ballW ballH 20
> 
>   color $ Color4 0.3 0.4 0.8 (0.5 :: GLfloat)
>   drawRect playerX playerY playerW playerH
>
>   flush
>   swapBuffers
>
> readInput mousePos mouseBut = do
>   t <- get GLFW.time
>   GLFW.time $= 0
>   Position x y <- get GLFW.mousePos
>   mousePos (V (fromIntegral x) (fromIntegral y))
>   b <- GLFW.getMouseButton GLFW.ButtonLeft
>   mouseBut (b == GLFW.Press)
>   k <- getKey ESC
>   return (if k == Press then Nothing else Just t)
> 
> initGL width height = do
>   clearColor $= Color4 0 0 0 1
>   blend $= Enabled
>   blendFunc $= (SrcAlpha,OneMinusSrcAlpha)
>   cullFace $= Just Back
> 
> resizeGLScene winSize size@(Size w h) = do
>   winSize (V (fromIntegral w) (fromIntegral h))
> 
>   viewport $= (Position 0 0,size)
> 
>   matrixMode $= Projection
>   loadIdentity
>   scale 1 (fromIntegral w/fromIntegral h) (1 :: GLfloat)
> 	
>   matrixMode $= Modelview 0
