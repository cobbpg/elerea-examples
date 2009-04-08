> module Common.Utils where
>
> import Control.Applicative
> import Control.Monad
> import FRP.Elerea
>
> import Common.Vector
> 
> driveNetwork network driver = do
>   dt <- driver
>   case dt of
>     Just dt -> do join $ superstep network dt
>                   driveNetwork network driver
>     Nothing -> return ()
> 
> edge b = (==(True,False)) <$> (transfer (True,True) (\dt b1 (b0,_) -> (b1,b0)) b)
> 
> integral v0 s = transfer v0 (\dt v v0 -> v0+v*realToFrac dt) s
>
> integralVec v0 s = transfer v0 (\dt v v0 -> v0^+^(v^*.realToFrac dt)) s
