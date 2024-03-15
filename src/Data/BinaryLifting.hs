{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

-- | Type class-based binary lifting.
--
-- Binary lifting is a technique for calculating nth power of a semigroup element in a (big)
-- constant time, or applying them to their semigroup action target.
--
-- The i-th element of the underlying vector of `BinaryLifting` stores \(s^{2^i}\), with which we
-- can construct any of \(s^i\) (\(0 <= i < 2^63\)) in a big (63) constant time.
module Data.BinaryLifting where

import Data.Bits
import Data.Core.SemigroupAction
import Data.Maybe
import Data.Monoid
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

-- | Binary lifting.
class BinaryLifting a where
  type VecBL a
  cacheBL :: a -> VecBL a

{-# INLINE cacheBLU #-}
cacheBLU :: (Semigroup a, U.Unbox a) => a -> U.Vector a
cacheBLU = U.iterateN 63 (\x -> x <> x)

{-# INLINE cacheBLV #-}
cacheBLV :: (Semigroup a) => a -> V.Vector a
cacheBLV = V.iterateN 63 (\x -> x <> x)

{-# INLINE stimesBL #-}
stimesBL :: (Semigroup a, G.Vector v a) => v a -> Int -> a -> a
stimesBL cache n !s0 = U.foldl' step s0 (U.generate 63 id)
  where
    {-# INLINE step #-}
    step !s i
      | testBit n i = let !s' = s <> cache G.! i in s'
      | otherwise = s

{-# INLINE mtimesBL #-}
mtimesBL :: (Monoid a, G.Vector v a) => v a -> Int -> a
mtimesBL cache n = stimesBL cache n mempty

{-# INLINE sactBL #-}
sactBL :: (SemigroupAction a b, G.Vector v a) => v a -> Int -> b -> b
sactBL cache n !b0 = U.foldl' step b0 (U.generate 63 id)
  where
    {-# INLINE step #-}
    step !b i
      | testBit n i = let !b' = cache G.! i `sact` b in b'
      | otherwise = b

----------------------------------------------------------------------------------------------------
-- Product
----------------------------------------------------------------------------------------------------

instance SemigroupAction (Product Int) Int where
  {-# INLINE sact #-}
  sact (Product !x1) !x2 = x1 * x2

instance (Num a, U.Unbox a) => BinaryLifting (Product a) where
  type VecBL (Product a) = U.Vector (Product a)
  {-# INLINE cacheBL #-}
  cacheBL = cacheBLU

----------------------------------------------------------------------------------------------------
-- Permutation
----------------------------------------------------------------------------------------------------

-- | Permutation of N sequence. `-1` does nothing.
--
-- = Typical problems
-- - [Typical 058 - Original Calculator (★4)](https://atcoder.jp/contests/typical90/tasks/typical90_bf)
newtype Permutation = Permutation (U.Vector Int)
  deriving (Show, Eq)

{-# INLINE unPermutation #-}
unPermutation :: Permutation -> U.Vector Int
unPermutation (Permutation vec) = vec

instance Semigroup Permutation where
  {-# INLINE (<>) #-}
  Permutation r2 <> Permutation r1 = Permutation $ U.map f r1
    where
      -- Preserving version:
      -- f i x1
      --   | x1 == -1 = i
      --   | x2 == -1 = x1
      --   | otherwise = x2
      --   where
      --     x2 = r2 U.! x1
      f x1
        | x1 == -1 = -1
        | otherwise = r2 U.! x1

instance SemigroupAction Permutation Int where
  {-# INLINE sact #-}
  -- sact (Permutation vec) i = case vec U.! i of
  --   (-1) -> i
  --   i' -> i'
  sact (Permutation vec) (-1) = -1
  sact (Permutation vec) i = vec U.! i

-- | The identity element of `Permutation`.
{-# INLINE idPerm #-}
idPerm :: Int -> Permutation
idPerm = Permutation . (`U.generate` id)

instance BinaryLifting Permutation where
  type VecBL Permutation = V.Vector Permutation
  {-# INLINE cacheBL #-}
  cacheBL = cacheBLV

----------------------------------------------------------------------------------------------------
-- TransiteSemigroup: Permutation + Semigroup
----------------------------------------------------------------------------------------------------

-- | Transition + semigroup concatanation. LCA is based on this.
newtype TransiteSemigroup a = TransiteSemigroup (U.Vector (Int, a))
  deriving (Show, Eq)

{-# INLINE unTransiteAction #-}
unTransiteAction :: TransiteSemigroup a -> U.Vector (Int, a)
unTransiteAction (TransiteSemigroup vec) = vec

instance (U.Unbox a, Semigroup a) => Semigroup (TransiteSemigroup a) where
  {-# INLINE (<>) #-}
  TransiteSemigroup r2 <> TransiteSemigroup r1 = TransiteSemigroup $ U.map f r1
    where
      {-# INLINE f #-}
      f (-1, !a) = (-1, a)
      f (!i, !a) =
        let (!i', !a') = r2 U.! i
            !a'' = a' <> a
         in (i', a'')

-- | @b@ as target
instance (U.Unbox a, SemigroupAction a b) => SemigroupAction (TransiteSemigroup a) (Int, b) where
  {-# INLINE sact #-}
  sact (TransiteSemigroup _) (-1, !b) = (-1, b)
  sact (TransiteSemigroup vec) (!i, !b) =
    let (!i', !a) = vec U.! i
        !b' = a `sact` b
     in (i', b')

-- -- | Self as target
-- Overlaping instance
-- instance (U.Unbox a, Semigroup a) => SemigroupAction (TransiteSemigroup a) (Int, a) where
--   {-# INLINE sact #-}
--   sact (TransiteSemigroup _) (-1, !b) = (-1, b)
--   sact (TransiteSemigroup vec) (!i, !b) =
--     let (!i', !a) = vec U.! i
--         !b' = a <> b
--      in (i', b')

-- | The identity element of `TransiteSemigroup`.
{-# INLINE idTransiteSemigroup #-}
idTransiteSemigroup :: (U.Unbox a) => Int -> a -> TransiteSemigroup a
idTransiteSemigroup !n !ident = TransiteSemigroup $ U.generate n (,ident)

instance (U.Unbox a, Semigroup a) => BinaryLifting (TransiteSemigroup a) where
  type VecBL (TransiteSemigroup a) = V.Vector (TransiteSemigroup a)
  {-# INLINE cacheBL #-}
  cacheBL = cacheBLV

-- -- | Storage of \(s^{2^i}\).
-- newtype BinaryLifting v m = BinaryLifting (v m)
--   deriving (Show, Eq)
--
-- -- | Calculates `BinaryLifting` of the given semigroup
-- newBinLift :: (G.Vector v s, Semigroup s) => s -> BinaryLifting v s
-- newBinLift !op0 = BinaryLifting ops
--   where
--     !ops = G.iterateN (pred 63) (\ !op -> op <> op) op0
--
-- -- | Calculates `BinaryLifting` of the given semigroup
-- newBinLiftV :: (Semigroup s) => s -> BinaryLifting V.Vector s
-- newBinLiftV = newBinLift
--
-- -- | Calculates `BinaryLifting` of the given semigroup
-- newBinLiftU :: (Semigroup s, U.Unbox s) => s -> BinaryLifting U.Vector s
-- newBinLiftU = newBinLift
--
-- -- | Binarily lifted version of `stimesMonoid`.
-- -- WARNING: Usually `sactBL` is much cheaper for semigroup actions with a boxed type.
-- stimesBL :: (HasCallStack, Semigroup s, G.Vector v s) => BinaryLifting v s -> s -> Int -> s
-- stimesBL (BinaryLifting !ops) !s0 !n = U.foldl' step s0 (U.enumFromN 0 62)
--   where
--     step !m !i
--       | testBit n i = m <> ops G.! i
--       | otherwise = m
--
-- -- | Binarily lifted version of `stimesMonoid`.
-- -- WARNING: Usually `sactBL` is much cheaper for semigroup actions with a boxed type.
-- mtimesBL :: (HasCallStack, Monoid m, G.Vector v m) => BinaryLifting v m -> Int -> m
-- mtimesBL !bin !n = stimesBL bin mempty n
--
-- -- | Binarily lifted semigroup action application.
-- sactBL :: (HasCallStack, SemigroupAction s a, G.Vector v s) => BinaryLifting v s -> a -> Int -> a
-- sactBL (BinaryLifting !ops) !acc0 !nAct = U.foldl' step acc0 (rangeG 0 62)
--   where
--     step !acc !nBit
--       | testBit nAct nBit = (ops G.! nBit) `sact` acc
--       | otherwise = acc
--
-- -- | Alias of `sactBL` for monoid action.
-- mactBL :: (HasCallStack, MonoidAction m a, G.Vector v m) => BinaryLifting v m -> a -> Int -> a
-- mactBL = sactBL
--
-- -- | Old binary lifting implementation without typeclasses. TODO: Remove
-- newDoubling :: (G.Vector v a, G.Vector v Int) => a -> (a -> a) -> v a
-- newDoubling !oper0 !squareCompositeF = G.scanl' step oper0 $ G.enumFromN (1 :: Int) 62
--   where
--     step !oper !_ = squareCompositeF oper
--
-- -- | Old binary lifting implementation without typeclasses. TODO: Remove
-- newDoublingV :: a -> (a -> a) -> V.Vector a
-- newDoublingV = newDoubling
--
-- -- | Old binary lifting implementation without typeclasses. TODO: Remove
-- applyDoubling :: (HasCallStack, G.Vector v op) => v op -> a -> (a -> op -> a) -> Int -> a
-- applyDoubling !opers !x0 !act !n = foldl' step x0 [0 .. 62]
--   where
--     !_ = dbgAssert $ G.length opers == 63
--     step !acc !nBit =
--       if testBit n nBit
--         then acc `act` (opers G.! nBit)
--         else acc
