> module Main where
> 
> import Control.Applicative
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
>   driveNetwork (chase mousePress mousePosition windowSize)
>                (readInput mousePositionSink mousePressSink)
> 
>   closeWindow
> 
> chase mousePress mousePos windowSize = render <$> windowSize <*> mousePos <*> ballPos
>     where ballPos = integralVec vnull ballVel
>           ballVel = latcher (integralVec vnull ballAcc)
>                             (edge mousePress)
>                             (integralVec <$> ballPos^-^mousePos <*> pure ballAcc)
>           ballAcc = (mousePos^-^ballPos)^*.0.3
> 
> render (V w h) (V cx cy) (V ox oy) = do
>   let drawSquare x y s = do
>         loadIdentity
>         translate $ Vector3 (x/w*2-1) (h/w-y/w*2) 0
>         renderPrimitive Quads $ do
>           vertex $ Vertex3 (-s) (-s) (0 :: GLfloat)
>           vertex $ Vertex3 ( s) (-s) (0 :: GLfloat)
>           vertex $ Vertex3 ( s) ( s) (0 :: GLfloat)
>           vertex $ Vertex3 (-s) ( s) (0 :: GLfloat)
> 
>   clear [ColorBuffer]
>  
>   color $ Color4 1 0 0 (0.5 :: GLfloat)
>   drawSquare cx cy 0.05
>   color $ Color4 1 1 1 (0.6 :: GLfloat)
>   drawSquare ox oy 0.03
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
