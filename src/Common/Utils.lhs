Utils module
============

This module contains some functions that might make it into the core library eventually.

> module Common.Utils where
>
> import Control.Applicative
> import Control.Monad
> import FRP.Elerea
>
> import Common.Vector

The `driveNetwork` function simply executes the supersteps while the
`driver` function keeps returning valid delta time values.

> driveNetwork network driver = do
>   dt <- driver
>   case dt of
>     Just dt -> do join $ superstep network dt
>                   driveNetwork network driver
>     Nothing -> return ()

The `edge` transfer function takes a bool signal and emits another
bool signal that turns true only at the moment when there is a rising
edge on the input.  We are relying on the delay introduced by the
`transfer` primitive for this to work.

> edge b = (transfer False (\dt b _ -> not b) b) &&@ b

A scalar integral function.

> integral v0 s = transfer v0 (\dt v v0 -> v0+v*realToFrac dt) s

An integral function for two-dimensional vectors defined in the `Vector` module.

> integralVec v0 s = transfer v0 (\dt v v0 -> v0^+^(v^*.realToFrac dt)) s

Logic relations lifted into signals.  Useful to combine event-like signals.

> (||@) :: Signal Bool -> Signal Bool -> Signal Bool
> (||@) = liftA2 (||)
> 
> (&&@) :: Signal Bool -> Signal Bool -> Signal Bool
> (&&@) = liftA2 (&&)
