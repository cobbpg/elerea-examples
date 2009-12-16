Elerea Breakout example
=======================

Breakout is one of the simplest test applications that brings up some
problems with non-trivial solutions.  In particular, there is a
circular dependency between the position and the velocity of the ball.
The position is the integral of the velocity, while velocity can have
instantaneous changes due to collisions, which is a function of the
position of the ball and the current state of the field.  The field is
dynamic, and it also forms a mutual dependency loop with the ball
position.  On top of these, part of the field (at least if we regard
the paddle as part of it) depends on user input, and its effects start
propagating in the game state as soon as the ball reaches the level of
the paddle.

Elerea lets us express all these circular dependencies in a natural
way, by simply referring to the respective signals by their names.
User input is also seen as a signal from inside the reactive part,
while the imperative framework is given a sink fuction to update it as
needed.  Type safety is ensured all the way.

<img src="Breakout.png" alt="Elerea Breakout in action" />

You can also have the program output the signal structure in
[Graphviz](http://www.graphviz.org/) dot format with the `--dump-dot`
switch after the game is over.  For instance, if Graphviz is
installed, you can get an SVG rendition of the graph using the
following command:

`elerea-breakout --dump-dot | dot -Tsvg -o breakout.svg`

Below follows the full source of the example.

> {-# LANGUAGE RecursiveDo #-}
>
> module Main where
>
> import Control.Applicative
> import Control.Concurrent
> import Control.Monad
> import Data.IORef
> import Data.List
> import Data.Maybe
> import Data.Traversable hiding (mapM)
> import FRP.Elerea
> import FRP.Elerea.Graph
> import Graphics.UI.GLFW as GLFW
> import Graphics.Rendering.OpenGL
> import System.Environment
> import System.IO.Unsafe

> import Common.Utils
> import Common.Vector

Global constants
----------------

The dimensions of the ball, which behaves as a rectangle in collision
detection, but is drawn as an ellipse.

> ballW = 0.04
> ballH = 0.04

The initial position and velocity of the ball.

> ballPos0 = V 0 (-0.4)
> ballVel0 = V (-0.4) (0.35)

The dimensions and vertical position of the player.

> playerW = 0.2
> playerH = 0.03
> playerY = -fieldH+0.01

The dimensions of the field.

> fieldW = 0.8
> fieldH = 0.5

The dimensions of each brick.

> brickW = 0.05
> brickH = 0.03

The data structure describing the state of each brick.  A brick can
either be alive or dying.  Dying bricks also keep track of their
fadeout level.

> data BrickState = Live | Dying !GLfloat deriving (Eq,Show)

The starting positions of the bricks.

> brickPos0 = distributeBricks (-0.7) (-0.1) (0.7) (0.4) 18 10
>     where distributeBricks xmin ymin xmax ymax xn yn = [(xmin+xstep*x,ymin+ystep*y) |
>                                                         x <- [0..xn-1], y <- [0..yn-1]]
>               where xstep = (xmax-xmin-xn*brickW)/(xn-1)+brickW
>                     ystep = (ymax-ymin-yn*brickH)/(yn-1)+brickH

The rate at which a brick fades out.  The reciprocal of this value
gives the fadeout time in seconds.

> brickFade = 0.5

Game logic
----------

The entry point performs some lightweight initialisation, and defines
the two user-driven signals: window size and mouse position.  The
`external` function creates the signal and the corresponding sink at
the same time.

When all is done `driveNetwork` is invoked.  It is not a library
function, but part of the tiny `Utils` module .

> main = do
>   -- Creating a window without a depth buffer
>   initialize
>   openWindow (Size 640 480) [DisplayRGBBits 8 8 8, DisplayAlphaBits 8] Window
>   windowTitle $= "Elerea Breakout"
>
>   -- External signals available for the game logic
>   (windowSize,windowSizeSink) <- external vnull
>   (mousePosition,mousePositionSink) <- external vnull
>
>   -- Wrapping up the init phase
>   closed <- newIORef False
>   windowSizeCallback $= resizeGLScene windowSizeSink
>   windowCloseCallback $= writeIORef closed True
>   initGL 640 480
>
>   -- All we need to get going is an IO-valued signal and an IO
>   -- function to update the external signals
>   game <- createSignal $ breakout mousePosition windowSize
>   driveNetwork game (readInput mousePositionSink closed)
>
>   -- The inevitable sad ending
>   closeWindow
>
>   -- Providing input for graphviz
>   args <- getArgs
>   when ("--dump-dot" `elem` args) $ do
>     gameGraph <- signalToDot game
>     putStr gameGraph

The `breakout` function creates a reactive signal that carries the
rendering actions to be performed at each instant.  The principal
signals forming the game logic are the following:

* `playerX`: the position of the player, a direct function of the
   mouse position;
* `ballPos`: the position of the ball given as an integral of its
   velocity, `ballVel`;
* `bricks`: the collection of live and dying bricks along with
   collision information.

The position and velocity of the ball form a circular dependency
through the bricks, as velocity is changed whenever a collision is
detected, which is a function of the position.

The signal carrying the collection of the bricks is a higher-order
signal, where each element of the list is a signal representing an
individual brick.  Bricks behave independently of each other: they are
defined as separate transfer functions with the ball position as input
signal.  As soon as a brick is touched it enters the dying phase and
fades out.  Also, since other signals are mostly interested in the
current state of the bricks, we have to define a flattened version,
which carries the snapshots of all the brick signals.  This is the
`brickSamples` signal.

> breakout mousePos windowSize = mdo

User-driven player position:

>   let playerX = adjustPlayerPos <$> mousePos <*> windowSize
>       adjustPlayerPos (V x _) (V w _) = min (fieldW-playerW) $ max (-fieldW) $ 2*x/w-1-playerW/2

Ball state: position and velocity.  We use a combination of
`storeJust` and `toMaybe` to produce a latcher element that stores the
value of a certain signal whenever a boolean control signal yields
true.

>   ballPos <- integralVec ballPos0 ballVel
>   ballVel <- storeJust ballVel0 $
>              toMaybe <$> (ballCollHorz ||@ ballCollVert ||@ ballCollPlayer) <*>
>                          (adjustVel <$> ballCollHorz <*> ballCollVert <*> ballCollPlayer <*>
>                           ballVel <*> ballNewVelX)

The `adjustVel` function calculates a candidate velocity for the next
frame given collision information and the current velocity.  Even
though it would return the current speed if there are no collisions,
we don't evaluate it at all thanks to the laziness of applicative
nodes.  In the end, velocity is only recalculated when a collision is
detected.

>   let adjustVel ch cv cp (V bvx bvy) bvx' = V x y
>           where x = (if ch then -1 else 1)*(if cp then bvx'*4 else bvx)
>                 y = if cv || cp then -bvy else bvy
>       ballNewVelX = (getX <$> ballPos)-playerX-pure (playerW/2)

Collision events are modelled with bool signals that turn true while
the ball overlaps the offending surface and approaches it at the same
time.  Collision response will make sure that the second condition
does not hold in the next instant, so there is no need to push these
through an `edge` transfer function.

>       ballCollHorz = (or . map getBrickHColl <$> brickSamples)
>                      ||@ (check <$> ballPos <*> ballVel)
>           where check (V bx _) (V bvx _) = (bx < -fieldW && bvx < 0) ||
>                                            (bx > fieldW-ballW && bvx > 0)
>       ballCollVert = (or . map getBrickVColl <$> brickSamples)
>                      ||@ (check <$> ballPos <*> ballVel)
>           where check (V _ by) (V _ bvy) = by > fieldH-ballH && bvy > 0
>       ballCollPlayer = check <$> ballPos <*> ballVel <*> playerX
>           where check (V bx by) (V _ bvy) px = bvy < 0 &&
>                    doRectsIntersect bx by ballW ballH px playerY playerW playerH

Bricks are defined by the updater function `evolveBrick` as a
transformer of the ball position.  The transfer function takes care of
fading and checking collision.  Collision information is part of the
state of the transfer function, even though it is strictly a function
of the brick data and the ball position at the moment.  However, since
we need to check collisions in order to update the state of the brick,
it's simpler and more efficient to let the outer world see the results
of these checks instead of having to recalculate them.

>   let brick (x,y) = transfer (x,y,Live,False,False) evolveBrick ballPos
>       getBrickData (x,y,s,_,_) = (x,y,s)
>       getBrickHColl (_,_,_,c,_) = c
>       getBrickVColl (_,_,_,_,c) = c
>
>       evolveBrick dt _   (x,y,Dying a,_,_) = (x,y,Dying (a-realToFrac dt*brickFade),False,False)
>       evolveBrick dt (V bx by) (x,y,_,_,_) = (x,y,if isKilled then Dying 1 else Live,collHorz,collVert)
>           where isKilled = isHit || by < -fieldH-ballH
>                 isHit = doRectsIntersect bx by ballW ballH x y brickW brickH
>                 collHorz = isHit && isHorz
>                 collVert = isHit && not isHorz
>                 isHorz = xDist/brickW > yDist/brickH
>                     where xDist = abs ((x+brickW/2)-(bx+ballW/2))
>                           yDist = abs ((y+brickH/2)-(by+ballH/2))

The `isBrickNeeded` function is used to decide whether a brick should
be kept in the collection.  As soon as it turns false, the brick in
question is removed from the `bricks` signal.

>       isBrickNeeded (_,_,Dying a,_,_) = a > 0
>       isBrickNeeded (_,_,Live   ,_,_) = True

The `brickSamples` signal contains a snapshot of every brick, and it's
obtained simply by traversing the collection (this is equivalent to
rebuilding the structure with lifted constructors), then applying a
sampler, which collapses the two signal layers into one.  We take
advantage of the fact that lists are instances of Traversable.

>       brickSamples = sampler (sequenceA <$> bricks)

The `bricks` signal carries the dynamic list of bricks along with
ball-brick collision information, all of which are updated in each
frame.  We start out with a number of live bricks in the positions
given by the `brickPos0` list, and derive the list of the next frame
from the current one by filtering out the bricks for which
`isBrickNeeded` evaluates to false.  These updates are made explicit
by using `delay` to define the dynamic collection.

>   bricks <- do
>     bricksInit <- mapM brick brickPos0
>     let bricksNext = map snd . filter (isBrickNeeded . fst) <$> (zip <$> brickSamples <*> bricks)
>     delay bricksInit bricksNext

And knowing all these signals we can finally assemble the signal of
rendering actions, i.e. the animation:

>   return $ renderLevel <$> playerX <*> ballPos <*> (map getBrickData <$> brickSamples)

The `doRectsIntersect` function decides whether two rectangles defined
by their top left corners and dimensions overlap.

> doRectsIntersect x1 y1 sx1 sy1 x2 y2 sx2 sy2 = collIV x1 sx1 x2 sx2 && collIV y1 sy1 y2 sy2
>     where collIV p1 s1 p2 s2 = (p1 <= p2 && p2 <= p1+s1) || (p2 <= p1 && p1 <= p2+s2)

The `renderLevel` function takes a snapshot of the game and turns it
into an IO action that displays this snapshot on the screen.  The
`breakout` signal is the time-varying version of this IO action.

> renderLevel playerX (V ballX ballY) bricks = do
>   let drawRect x y xs ys = do
>         loadIdentity
>         renderPrimitive Quads $ do
>           vertex $ Vertex3 (x)    (y)    (0 :: GLfloat)
>           vertex $ Vertex3 (x+xs) (y)    (0 :: GLfloat)
>           vertex $ Vertex3 (x+xs) (y+ys) (0 :: GLfloat)
>           vertex $ Vertex3 (x)    (y+ys) (0 :: GLfloat)
>       drawEllipse x y xs ys n = do
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
>   forM_ bricks $ \(x,y,s) -> do
>     case s of
>       Live    -> color $ Color4 0.8 0.5 0.5 (0.6 :: GLfloat)
>       Dying a -> color $ Color4 0.9 0.9 0.2 a
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

Backend
-------

The `readInput` function has two responsibilities: it provides input
for the `mousePosition` peripheral signal through its associated sink,
and also feeds the time difference between two states into the system,
deciding when to exit altogether (by returning `Nothing` instead of
the current `dt` value wrapped in `Just`).

The `threadDelay` call at the beginning is just a trick to give the
scheduler a breath.  It will cause a wait equal to a scheduler tick,
which is 20ms by default.  The program can run perfectly without it,
but it eats up all the free CPU to produce an unnecessarily high frame
rate.

> readInput mousePos closed = do
>   threadDelay 0
>   t <- get GLFW.time
>   GLFW.time $= 0
>   Position x y <- get GLFW.mousePos
>   mousePos (V (fromIntegral x) (fromIntegral y))
>   k <- getKey ESC
>   c <- readIORef closed
>   return (if c || k == Press then Nothing else Just t)

The `initGL` function sets up almost nothing, which means that most
functionality is turned off.  Only alpha blending is enabled to
provide some minimalistic eye candy.

> initGL width height = do
>   clearColor $= Color4 0 0 0 1
>   blend $= Enabled
>   blendFunc $= (SrcAlpha,OneMinusSrcAlpha)
>   cullFace $= Just Back

The resize callback feeds the `windowSize` signal through its sink
besides adjusting the projection matrix.

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
