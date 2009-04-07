> module Main where
> 
> import Control.Applicative
> import Control.Monad
> import FRP.Elerea
> import Graphics.UI.GLFW as GLFW
> import Graphics.Rendering.OpenGL
> 
> main = do
>   initialize
>   openWindow (Size 640 480) [DisplayRGBBits 8 8 8, DisplayAlphaBits 8, DisplayDepthBits 24] Window
>   windowTitle $= "Elerea Breakout"
> 
>   (windowSize,windowSizeSink) <- external (0,0)
>   (mousePosition,mousePositionSink) <- external (0,0)
>   (mousePress,mousePressSink) <- external False
> 
>   windowSizeCallback $= resizeGLScene windowSizeSink
>   initGL 640 480
> 
>   let followPosition = integralVec (0,0) followVelocity
>       followVelocity = latcher (integralVec (0,0) followAcceleration)
>                                 (edge mousePress)
>                                 ((\(vx0,vy0) -> integralVec (-vx0,-vy0) followAcceleration) <$> followVelocity)
>       followAcceleration = liftA2 (^*.) (liftA2 (^-^) mousePosition followPosition) (pure 0.3)
> 
>   driveNetwork (drawGLScene <$> windowSize <*> mousePosition <*> followPosition)
>                (readInput mousePositionSink mousePressSink)
> 
>   closeWindow
> 
> integralVec v0 s = transfer v0 (\dt v v0 -> v0^+^(v^*.realToFrac dt)) s
> 
> edge b = (==(True,False)) <$> (transfer (True,True) (\dt b1 (b0,_) -> (b1,b0)) b)
> 
> (x1,y1) ^+^ (x2,y2) = (x1+x2,y1+y2)
> (x1,y1) ^-^ (x2,y2) = (x1-x2,y1-y2)
> (x,y) ^*. t = (x*t,y*t)
> t .*^ (x,y) = (x*t,y*t)
> 
> readInput mousePos mouseBut = do
>   t <- get GLFW.time
>   GLFW.time $= 0
>   Position x y <- get GLFW.mousePos
>   mousePos (fromIntegral x,fromIntegral y)
>   b <- GLFW.getMouseButton GLFW.ButtonLeft
>   mouseBut (b == GLFW.Press)
>   k <- getKey ESC
>   return (if k == Press then Nothing else Just t)
> 
> driveNetwork network driver = do
>   dt <- driver
>   case dt of
>     Just dt -> do join $ superstep network dt
>                   driveNetwork network driver
>     Nothing -> return ()
> 
> initGL width height = do
>   clearColor $= Color4 0 0 0 1
>   --clearDepth $= 1
>   --depthFunc $= Just Less
>   --depthMask $= Enabled
>   --shadeModel $= Smooth
>   --position (Light 0) $= Vertex4 5 5 10 0
>   blend $= Enabled
>   blendFunc $= (SrcAlpha,OneMinusSrcAlpha)
>   cullFace $= Just Back
>   --lighting $= Disabled
>   --light (Light 0) $= Enabled
> 
> resizeGLScene winSize size@(Size w h) = do
>   winSize (fromIntegral w,fromIntegral h)
> 
>   viewport $= (Position 0 0,size)
> 
>   matrixMode $= Projection
>   loadIdentity
> 
>   scale 1 (fromIntegral w/fromIntegral h) (1 :: GLfloat)
>   --perspective 45 (fromIntegral w / fromIntegral h) 0.1 200
> 	
>   matrixMode $= Modelview 0
> 
> drawGLScene (w,h) (cx,cy) (ox,oy) = do
>   let drawSquare x y s = do
>         loadIdentity
>         translate $ Vector3 (x/w*2-1) (h/w-y/w*2) (0 :: GLfloat)
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
