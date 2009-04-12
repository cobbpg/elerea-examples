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

A scalar integral function.  Since it is based on `transfer`, it is
delayed by one superstep, i.e. the value of the input signal in the
n<sup>th</sup> step affects its output only in the (n+1)<sup>th</sup>
step.

> integral v0 s = transfer v0 (\dt v v0 -> v0+v*realToFrac dt) s

An integral function for two-dimensional vectors defined in the
`Vector` module with the same caveat.

> integralVec v0 s = transfer v0 (\dt v v0 -> v0^+^(v^*.realToFrac dt)) s
