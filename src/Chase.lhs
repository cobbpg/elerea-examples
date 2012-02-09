Elerea Chase example
====================

This is a minimal example to show how to define signals that can be
mutually recursive and can optionally depend on user input too.  The
grey square accelerates towards the red square at a rate proportional
to their relative position, and it can be given a momentary impulse
with the left mouse button.

For a slightly more complex example check out `Breakout.lhs`.

> {-# LANGUAGE DoRec #-}
>
> module Main where
>
> import Control.Applicative
> import Control.Monad
> import Data.IORef
> import FRP.Elerea.Param
> import Graphics.UI.GLFW as GLFW
> import Graphics.Rendering.OpenGL
>
> import Common.Utils
> import Common.Vector

The `main` function contains the whole reactive logic.  Note that
`driveNetwork` is just a simple loop, and you can see its source below
in the `Utils` module.

> main = do
>     initialize
>     openWindow (Size 640 480) [DisplayRGBBits 8 8 8, DisplayAlphaBits 8, DisplayDepthBits 24] Window
>     windowTitle $= "Elerea Chase"
>
>     (windowSize,windowSizeSink) <- external vnull
>     (mousePosition,mousePositionSink) <- external vnull
>     (mousePress,mousePressSink) <- external False
>
>     closed <- newIORef False
>     windowSizeCallback $= resizeGLScene windowSizeSink
>     windowCloseCallback $= (writeIORef closed True >> return True)
>     initGL 640 480
>
>     network <- start $ do
>         mouseClick <- edge mousePress
>         rec let newVel clk v0 = case clk of
>                     True -> Just <$> integralVec v0 acc
>                     False -> return Nothing
>                 acc = (mousePosition^-^pos)^*.0.3
>             vel0 <- integralVec vnull acc
>             vels <- storeJust vel0 =<< generator (newVel <$> mouseClick <*> vel^+^pos^-^mousePosition)
>             vel <- delay vnull (join vels)
>             pos <- delay vnull =<< integralVec vnull vel
>
>         return $ render <$> windowSize <*> mousePosition <*> pos
>
>     driveNetwork network (readInput mousePositionSink mousePressSink closed)
>
>     closeWindow

The `render` function takes a snapshot of the system (window size and
the positions of the squares) and turns it into OpenGL calls.  The
signal executed by the `driveNetwork` function is the time-varying
version of the IO action returned here.

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

The `readInput` function provides the driver layer.  It feeds the
peripheral-bound signals and also decides when to stop execution by
returning `Nothing` instead of the time elapsed since its last call.

> readInput mousePos mouseBut closed = do
>   t <- get GLFW.time
>   GLFW.time $= 0
>   Position x y <- get GLFW.mousePos
>   mousePos (V (fromIntegral x) (fromIntegral y))
>   b <- GLFW.getMouseButton GLFW.ButtonLeft
>   mouseBut (b == GLFW.Press)
>   k <- getKey ESC
>   c <- readIORef closed
>   return (if c || k == Press then Nothing else Just t)

OpenGL is initialised with practically everything turned off.  Only
alpha blending is needed to be able to use translucent colours.

> initGL width height = do
>   clearColor $= Color4 0 0 0 1
>   blend $= Enabled
>   blendFunc $= (SrcAlpha,OneMinusSrcAlpha)

The window size callback takes care of the `windowSize` signal and the
projection matrix.

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
