{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Multi set backed by `IntMap`.
module Data.BinaryLifting where

import qualified AtCoder.Internal.Assert as ACIA
import Data.Bits (testBit)
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import GHC.Stack (HasCallStack)
import Math.BitSet

-- | Binary lifting
cacheBL :: (Semigroup a) => Int -> a -> V.Vector a
cacheBL n s = V.constructN n $ \sofar ->
  if G.null sofar
    then s
    else
      let !s1 = G.last sofar
       in s1 <> s1

-- | Binary lifting
{-# INLINE stimesBL #-}
stimesBL :: (HasCallStack, Semigroup s, G.Vector v s) => v s -> Int -> s
stimesBL cache n = G.foldl' f (cache G.! b) bs
  where
    (!b, !bs) = fromJust . U.uncons $ bitsOf n
    f !a !iBit = a <> cache G.! iBit

-- | Binary lifting
{-# INLINE sactWithBL #-}
sactWithBL :: (HasCallStack, G.Vector v s) => v s -> (s -> a -> a) -> Int -> a -> a
sactWithBL cache sactF n a0
  | n == 0 = a0
  | otherwise = G.ifoldl' f a0 cache
  where
    f !a iBit !s
      | testBit n iBit = sactF s a
      | otherwise = a

newtype PermutationWithSemigroup a = PermutationWithSemigroup
  { unPermutationWithSemigroup :: U.Vector (Int, a)
  }
  deriving newtype (Eq, Show)

{-# INLINE newPWP #-}
newPWP :: (HasCallStack, U.Unbox a) => U.Vector (Int, a) -> PermutationWithSemigroup a
newPWP ixs = PermutationWithSemigroup ixs
  where
    n = U.length ixs
    !_ = U.foldl' (\() (!i, !_) -> let !_ = ACIA.runtimeAssert (-1 <= i && i < n) "newPWP: index boundary error" in ()) () ixs

-- | @since 1.1.0.0
instance (Semigroup a, U.Unbox a) => Semigroup (PermutationWithSemigroup a) where
  {-# INLINE (<>) #-}
  PermutationWithSemigroup p2 <> PermutationWithSemigroup p1 = PermutationWithSemigroup $ U.map f p1
    where
      !_ = ACIA.runtimeAssert (U.length p2 == U.length p1) "(<>): length mismatch"
      f (-1, !a) = (-1, a)
      f (i, !a) =
        let (!i', !aNew) = G.unsafeIndex p2 i
         in (i', aNew <> a)

{-# INLINE actPWP #-}
actPWP :: (HasCallStack, Semigroup a, U.Unbox a) => PermutationWithSemigroup a -> (Int, a) -> (Int, a)
actPWP (PermutationWithSemigroup vec) (!i, !a) = case vec G.! i of
  (-1, !_) -> (i, a)
  (i', !aOld) -> (i', a <> aOld)
