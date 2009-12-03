{-# LANGUAGE RecursiveDo, NoMonomorphismRestriction #-}

module Math where

import Control.Applicative
import Data.List
import FRP.Elerea.Experimental

import Vector
import Util

integral :: (Num a) => a -> Signal a a -> SignalGen a (Signal a a)
integral v0 s = transfer v0 (\dt v v0 -> v0+v*dt) s

integralV :: (Vector2D v c) => v -> Signal c v -> SignalGen c (Signal c v)
integralV v0 s = transfer v0 (\dt v v0 -> v0^+^(v^*.dt)) s

deriv :: (Fractional a) => Signal a a -> SignalGen a (Signal a a)
deriv s = do
  sig <- transfer (0,0,1) (\dt v (v0,_,_) -> (v,v0,dt)) s
  initSignal 0 (d <$> sig)
  where d (x',x,dt) = (x'-x)/dt

derivV :: (Vector2D v c, Num c) => Signal c v -> SignalGen c (Signal c v)
derivV s = do
  sig <- transfer (vnull,vnull,1) (\dt v (v0,_,_) -> (v,v0,dt)) s
  initSignal vnull (d <$> sig)
  where d (x',x,dt) = (x'^-^x)^/.dt

derivT :: (Fractional a, Ord a) => a -> Signal a a -> SignalGen a (Signal a a)
derivT wt s = do
  sig <- transfer (0,(0,0)) d s
  return (fst <$> sig)
  where d dt v (x,(v0,t)) = if t' > wt then ((v-v0)/t',(v,0)) else (x,(v0,t'))
          where t' = dt+t

derivTV :: (Vector2D v c, Ord c, Num c) => c -> Signal c v -> SignalGen c (Signal c v)
derivTV wt s = do
  sig <- transfer (vnull,(vnull,0)) d s
  return (fst <$> sig)
  where d dt v (x,(v0,t)) = if t' > wt then ((v^-^v0)^/.t',(v,0)) else (x,(v0,t'))
          where t' = dt+t

movingAvg :: (Fractional a) => a -> Signal p a -> SignalGen p (Signal p a)
movingAvg n s = do
  sig <- scanM n (delay 0) s
  return $ ((/n).sum) <$> sequence sig

movingAvgV :: (Vector2D v c, Num c) => c -> Signal p v -> SignalGen p (Signal p v)
movingAvgV n s = do
  sig <- scanM n (delay vnull) s
  return $ ((^/.n).foldl1' (^+^)) <$> sequence sig
