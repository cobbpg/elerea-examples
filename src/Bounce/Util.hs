module Util where

import Control.Applicative
import FRP.Elerea.Experimental

ifS c s1 s2 = c >>= \b -> if b then s1 else s2

initSignal x s = do
  c <- stateful True (const (const False))
  return $ ifS c (pure x) s

scanM 0 _ _ = return []
scanM n f m = do
  x <- f m
  xs <- scanM (n-1) f x
  return (x:xs)
