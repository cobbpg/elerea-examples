{-# LANGUAGE RecursiveDo #-}

module Event where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Maybe
import FRP.Elerea.Experimental

import Util

{-| Events are signals with an option type. -}
type Event p a = Signal p (Maybe a)

{-| Sample a signal whenever a changing condition is true. -}
ifE :: Signal p Bool -> Signal p a -> Event p a
ifE c s = c >>= \b -> if b then Just <$> s else return Nothing

{-| Left-biased merge. -}
leftE :: Event p a -> Event p a -> Event p a
leftE e1 e2 = e1 >>= maybeE e2

{-| Right-biased merge. -}
rightE :: Event p a -> Event p a -> Event p a
rightE e1 e2 = e2 >>= maybeE e1

{-| Left-biased merge of several events. -}
mergeE :: [Event p a] -> Event p a
mergeE []     = return Nothing
mergeE (e:es) = e >>= maybeE (mergeE es)

{-| Maintain a changing list of entities fed in as events along with a
function to derive the corresponding signal of the removal condition
(the signal is removed when the associated removal signal is 'True' for
the first time). -}
collectE :: Event p a -> (a -> Signal p Bool) -> SignalGen p (Signal p [a])
collectE e f = mdo
  col <- delay [] col'
  col' <- dmemo [] $ filterM (fmap not . f) =<< liftM2 ((++).maybeToList) e col
  return col'

{-| A helper function equivalent to @flip maybe (return.Just)@. -}
maybeE :: Event p a -> Maybe a -> Event p a
maybeE e Nothing = e
maybeE _ jx      = return jx
