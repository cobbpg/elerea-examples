module Util where

import Control.Applicative
import FRP.Elerea.Delayed

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

-- Functions moved over from the old Experimental top module

infix  4 ==@, /=@, <@, <=@, >=@, >@
infixr 3 &&@
infixr 2 ||@
infix  2 -->

-- | The 'edge' transfer function takes a bool signal and emits
-- another bool signal that turns true only at the moment when there
-- is a rising edge on the input.
edge :: Signal p Bool -> SignalGen p (Signal p Bool)
edge b = delay True b >>= \db -> return $ (not <$> db) &&@ b

-- | The '-->' transfer function behaves as a latch on a 'Maybe'
-- input: it keeps its state when the input is 'Nothing', and replaces
-- it with the input otherwise.
(-->) :: a                        -- ^ Initial output
      -> Signal p (Maybe a)       -- ^ Maybe signal to latch on
      -> SignalGen p (Signal p a)
x0 --> s = transfer x0 store s
    where store _ Nothing  x = x
          store _ (Just x) _ = x

-- | Point-wise equality of two signals.
(==@) :: Eq a => Signal p a -> Signal p a -> Signal p Bool
(==@) = liftA2 (==)

-- | Point-wise inequality of two signals.
(/=@) :: Eq a => Signal p a -> Signal p a -> Signal p Bool
(/=@) = liftA2 (/=)

-- | Point-wise comparison of two signals.
(<@) :: Ord a => Signal p a -> Signal p a -> Signal p Bool
(<@) = liftA2 (<)

-- | Point-wise comparison of two signals.
(<=@) :: Ord a => Signal p a -> Signal p a -> Signal p Bool
(<=@) = liftA2 (<=)

-- | Point-wise comparison of two signals.
(>=@) :: Ord a => Signal p a -> Signal p a -> Signal p Bool
(>=@) = liftA2 (>=)

-- | Point-wise comparison of two signals.
(>@) :: Ord a => Signal p a -> Signal p a -> Signal p Bool
(>@) = liftA2 (>)

-- | Point-wise OR of two boolean signals.
(||@) :: Signal p Bool -> Signal p Bool -> Signal p Bool
s1 ||@ s2 = s1 >>= \b -> if b then return True else s2

-- | Point-wise AND of two boolean signals.
(&&@) :: Signal p Bool -> Signal p Bool -> Signal p Bool
s1 &&@ s2 = s1 >>= \b -> if b then s2 else return False
