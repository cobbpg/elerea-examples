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

Below follows the full source of the example.

> module Main where
> 
> import Control.Applicative
> import Control.Concurrent
> import Control.Monad
> import Data.List
> import Data.Maybe
> import FRP.Elerea
> import Graphics.UI.GLFW as GLFW
> import Graphics.Rendering.OpenGL
> 
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

The starting positions of the bricks.  The third element of the
triples is the state of the brick.  A brick can either be alive or
dying.  Dying bricks also keep track of their fadeout level.

> data BrickState = Live | Dying Double deriving Eq
>
> brickPos0 = distributeBricks (-0.7) (-0.1) (0.7) (0.4) 18 10
>     where distributeBricks xmin ymin xmax ymax xn yn = [(xmin+xstep*x,ymin+ystep*y,Live) |
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
>   windowSizeCallback $= resizeGLScene windowSizeSink
>   initGL 640 480
> 
>   -- All we need to get going is an IO-valued signal and an IO
>   -- function to update the external signals
>   driveNetwork (breakout mousePosition windowSize)
>                (readInput mousePositionSink)
> 
>   -- The inevitable sad ending
>   closeWindow

The `breakout` function creates a reactive signal that carries the
rendering actions to be performed at each instant.  The principal
signals forming the game logic are the following:

* `playerX`: the position of the player, a direct function of the
   mouse position;
* `ballPos`: the position of the ball given as an integral of its
   velocity, `ballVel`;
* `bricks`: the collection of live and dying bricks along with
   collision information.

The position and velocity of the ball form a circular dependency, as
velocity is changed whenever a collision is detected, which is a
function of the position.

The signal carrying the collection of the bricks is obtained by
applying a stateful transformation (a transfer function) on the ball
position.  The state contains the bricks still relevant to the game as
well as collision information.  Although the latter is technically
redundant, it is still worth including here, since it has to be
calculated while advancing the state of the bricks, and it is needed
elsewhere too.

> breakout mousePos windowSize = renderLevel <$> playerX <*> ballPos <*> (getBricks <$> bricks)
>     where 

User-driven player position:

>           playerX = adjustPlayerPos <$> mousePos <*> windowSize
>           adjustPlayerPos (V x _) (V w _) = min (fieldW-playerW) $ max (-fieldW) $ 2*x/w-1-playerW/2

Ball state: position and velocity.  Note the use of the latcher: we
wrap a signal created by `adjustVel` in a `pure`, hence adding another
signal layer around it.

>           ballPos = integralVec (ballPos0) ballVel
>           ballVel = latcher (pure ballVel0)
>                             (ballCollHorz ||@ ballCollVert ||@ ballCollPlayer)
>                             (pure <$> (adjustVel <$> ballCollHorz <*> ballCollVert <*> ballCollPlayer <*>
>                                        ballVel <*> ballNewVelX))

The `adjustVel` function calculates a candidate velocity for the next
frame given collision information and the current velocity.  Even
though it would return the current speed if there are no collisions,
we rather not evaluate it at all thanks to the latcher.  In the end,
velocity is not recalculated in each frame.

>           adjustVel ch cv cp (V bvx bvy) bvx' = V x y
>               where x = (if ch then -1 else 1)*(if cp then bvx'*4 else bvx)
>                     y = if cv || cp then -bvy else bvy
>           ballNewVelX = (getX <$> ballPos)-playerX-pure (playerW/2)

Collision events are modelled with bool signals that turn true while
the ball overlaps the offending surface and approaches it at the same
time.  Collision response will make sure that the second condition
does not hold in the next instant, so there is no need to push these
through an `edge` transfer function.

>           ballCollHorz = (getHColl <$> bricks) ||@ (check <$> ballPos <*> ballVel)
>               where check (V bx _) (V bvx _) = (bx < -fieldW && bvx < 0) ||
>                                                (bx > fieldW-ballW && bvx > 0)
>           ballCollVert = (getVColl <$> bricks) ||@ (check <$> ballPos <*> ballVel)
>               where check (V bx by) (V _ bvy) = by > fieldH-ballH && bvy > 0
>           ballCollPlayer = check <$> ballPos <*> ballVel <*> playerX
>               where check (V bx by) (V _ bvy) px = bvy < 0 && doRectsIntersect bx by ballW ballH
>                                                                       px playerY playerW playerH
>

The dynamic list of bricks along with ball-brick collision
information, all of which are updated in each frame.

>           bricks = transfer (False,False,brickPos0) updateBricks ballPos
>           getBricks (_,_,bs) = bs
>           getHColl (c,_,_) = c
>           getVColl (_,c,_) = c

Brick updater function: fading and checking collision.  We need to do
this the traditional way, because Elerea does not provide constructs
to deal with collections of signals in the reactive layer, unlike
Yampa.

>           updateBricks dt (V bx by) (_,_,bricksPrev) = (collHorz,collVert,bricksNext)
>               where bricksNext = catMaybes (map evolveBrick (map killBrick bricksDel++bricksRem))
>                     (bricksDel,bricksNotHit) = partition classifyBrick bricksPrev
>                     bricksRem = if by > -fieldH-ballH then bricksNotHit else map killBrick bricksNotHit
>                     classifyBrick (x,y,s) = s == Live && killed
>                         where killed = doRectsIntersect bx by ballW ballH x y brickW brickH
>                     evolveBrick (x,y,Dying a) = if a < fade then Nothing else Just (x,y,Dying (a-fade))
>                         where fade = realToFrac dt*brickFade
>                     evolveBrick b = Just b
>                     killBrick (x,y,Live) = (x,y,Dying 1)
>                     killBrick b = b
>                     collHorz = or collHBricks
>                     collVert = or (map not collHBricks)
>                     collHBricks = map isHorz bricksDel
>                     isHorz (x,y,_) = xDist/brickW > yDist/brickH
>                         where xDist = abs ((x+brickW/2)-(bx+ballW/2))
>                               yDist = abs ((y+brickH/2)-(by+ballH/2))

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

> readInput mousePos = do
>   threadDelay 0
>   t <- get GLFW.time
>   GLFW.time $= 0
>   Position x y <- get GLFW.mousePos
>   mousePos (V (fromIntegral x) (fromIntegral y))
>   k <- getKey ESC
>   return (if k == Press then Nothing else Just t)

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
