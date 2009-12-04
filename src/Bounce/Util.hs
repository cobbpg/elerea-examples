module Util where

import Control.Applicative
import FRP.Elerea.Experimental

-- Lifted conditional.
ifS c s1 s2 = c >>= \b -> if b then s1 else s2

-- Override the first output of a signal.
initSignal x s = do
  c <- stateful True (const (const False))
  return $ ifS c (pure x) s

scanM 0 _ _ = return []
scanM n f m = do
  x <- f m
  xs <- scanM (n-1) f x
  return (x:xs)

-- Optional delay. Should memo behave like this?
dmemo = flip transfer (const const)
