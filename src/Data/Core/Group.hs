-- | Group is a monoid with inverse elements.
module Data.Core.Group where

import Data.Semigroup

-- | Group (S, *) is a monoid with inverse elements.
--
-- = Requirements
-- 1. There's an identity element \(id\): \(a \diamond id = id \diamond a = a\).
-- 2. \(\diamond\) is associative: \((a \diamond b) \diamond c = a \diamond (b \diamond c)\).
-- 3. There's an inverse lement: \(a a^{-1} = a^{-1} a = 1\).
class (Monoid a) => Group a where
  -- | Returns an inverse element.
  invert :: a -> a

instance (Num a) => Group (Sum a) where
  {-# INLINE invert #-}
  invert (Sum x) = Sum (-x)

instance (Group a) => Group (Dual a) where
  {-# INLINE invert #-}
  invert (Dual x) = Dual (invert x)

