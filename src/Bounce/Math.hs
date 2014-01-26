{-# LANGUAGE NoMonomorphismRestriction #-}

module Math where

import Control.Applicative
import Data.List
import FRP.Elerea.Param

import Vector
import Util

{-| Scalar integral. -}
integral :: (Num a) => a -> Signal a -> SignalGen a (Signal a)
integral v0 s = transfer v0 (\dt v v0 -> v0+v*dt) s

{-| Vector integral. -}
integralV :: (Vector2D v c) => v -> Signal v -> SignalGen c (Signal v)
integralV v0 s = transfer v0 (\dt v v0 -> v0^+^(v^*.dt)) s

{-| Scalar derivative by the last two values. -}
deriv :: (Fractional a) => Signal a -> SignalGen a (Signal a)
deriv s = do
  sig <- transfer (0,0,1) (\dt v (v0,_,_) -> (v,v0,dt)) s
  initSignal 0 (d <$> sig)
  where d (x',x,dt) = (x'-x)/dt

{-| Vector derivative by the last two values. -}
derivV :: (Vector2D v c, Num c) => Signal v -> SignalGen c (Signal v)
derivV s = do
  sig <- transfer (vnull,vnull,1) (\dt v (v0,_,_) -> (v,v0,dt)) s
  initSignal vnull (d <$> sig)
  where d (x',x,dt) = (x'^-^x)^/.dt

{-| Scalar derivative by a given time period. -}
derivT :: (Fractional a, Ord a) => a -> Signal a -> SignalGen a (Signal a)
derivT wt s = do
  sig <- transfer (0,(0,0)) d s
  return (fst <$> sig)
  where d dt v (x,(v0,t)) = if t' > wt then ((v-v0)/t',(v,0)) else (x,(v0,t'))
          where t' = dt+t

{-| Vector derivative by a given time period. -}
derivTV :: (Vector2D v c, Ord c, Num c) => c -> Signal v -> SignalGen c (Signal v)
derivTV wt s = do
  sig <- transfer (vnull,(vnull,0)) d s
  return (fst <$> sig)
  where d dt v (x,(v0,t)) = if t' > wt then ((v^-^v0)^/.t',(v,0)) else (x,(v0,t'))
          where t' = dt+t

{-| Scalar moving average of a given number of recent samples. -}
movingAvg :: (Eq a, Fractional a) => a -> Signal a -> SignalGen p (Signal a)
movingAvg n s = do
  sig <- scanM n (delay 0) s
  return $ ((/n).sum) <$> sequence sig

{-| Vector moving average of a given number of recent samples. -}
movingAvgV :: (Eq c, Vector2D v c, Num c) => c -> Signal v -> SignalGen p (Signal v)
movingAvgV n s = do
  sig <- scanM n (delay vnull) s
  return $ ((^/.n).foldl1' (^+^)) <$> sequence sig
