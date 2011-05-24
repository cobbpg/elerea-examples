{-# LANGUAGE DoRec #-}

module Event where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Maybe
import FRP.Elerea.Param

import Util

-- | Events are signals with an option type.
type Event a = Signal (Maybe a)

-- | Sample a signal whenever a changing condition is true.
ifE :: Signal Bool -> Signal a -> Event a
ifE c s = c >>= \b -> if b then Just <$> s else return Nothing

-- | Left-biased merge.
leftE :: Event a -> Event a -> Event a
leftE e1 e2 = e1 >>= maybeE e2

-- | Right-biased merge.
rightE :: Event a -> Event a -> Event a
rightE e1 e2 = e2 >>= maybeE e1

-- | Left-biased merge of several events.
mergeE :: [Event a] -> Event a
mergeE []     = return Nothing
mergeE (e:es) = e >>= maybeE (mergeE es)

-- | Maintain a changing list of entities fed in as events along with
-- a function to derive the corresponding signal of the removal
-- condition (the signal is removed when the associated removal signal
-- is 'True' for the first time).
collectE :: Event a -> (a -> Signal Bool) -> SignalGen p (Signal [a])
collectE e f = do
  rec col <- delay [] col'
      col' <- memo $ filterM (fmap not . f) =<< liftM2 ((++).maybeToList) e col
  return col'

-- | A helper function equivalent to @flip maybe (return.Just)@.
maybeE :: Event a -> Maybe a -> Event a
maybeE e Nothing = e
maybeE _ jx      = return jx
