module Event where

import Control.Applicative
import Control.Monad.Fix
import Data.Maybe
import FRP.Elerea.Experimental

{-| Events are signals with an option type. -}
type Event p a = Signal p (Maybe a)

ifE :: Signal p Bool -> Signal p a -> Event p a
ifE c s = c >>= \b -> if b then Just <$> s else return Nothing

leftE :: Event p a -> Event p a -> Event p a
leftE e1 e2 = e1 >>= maybeE e2

rightE :: Event p a -> Event p a -> Event p a
rightE e1 e2 = e2 >>= maybeE e1

mergeE :: [Event p a] -> Event p a
mergeE []     = return Nothing
mergeE (e:es) = e >>= maybeE (mergeE es)

collectE :: Event p a -> SignalGen p (Signal p [a])
collectE e = mfix $ \sig -> delay [] (((++).maybeToList) <$> e <*> sig)

{-| Equivalent to @flip maybe (return.Just)@. -}
maybeE :: Event p a -> Maybe a -> Event p a
maybeE e Nothing = e
maybeE _ jx      = return jx
