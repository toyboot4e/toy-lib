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

import Data.Core.SemigroupAction
import Data.Monoid
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import Math.BitSet (bitsOf)

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
stimesBL cache n !s0 = U.foldl' step s0 (bitsOf n)
  where
    {-# INLINE step #-}
    step !s i = let !s' = s <> cache G.! i in s'

{-# INLINE mtimesBL #-}
mtimesBL :: (Monoid a, G.Vector v a) => v a -> Int -> a
mtimesBL cache n = stimesBL cache n mempty

{-# INLINE sactBL #-}
sactBL :: (SemigroupAction a b, G.Vector v a) => v a -> Int -> b -> b
sactBL cache n !b0 = U.foldl' step b0 (bitsOf n)
  where
    {-# INLINE step #-}
    step !b i = let !b' = cache G.! i `sact` b in b'

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
-- TransiteSemigroup: Permutation + Semigroup
----------------------------------------------------------------------------------------------------

type Permutation = TransiteSemigroup ()

-- | The identity element of `Permutation`.
{-# INLINE idPerm #-}
idPerm :: Int -> Permutation
idPerm = TransiteSemigroup . (`U.generate` (,()))

-- | Transition + semigroup concatanation. LCA is based on this.
--
-- = Typical problems (Permutation)
-- - [Typical 058 - Original Calculator (★4)](https://atcoder.jp/contests/typical90/tasks/typical90_bf)
-- = Typical problems (Permutation)
--
-- = Typical problems (TransiteSemigroup)
-- c.f. LCA.
newtype TransiteSemigroup a = TransiteSemigroup (U.Vector (Int, a))
  deriving (Show, Eq)

{-# INLINE unTransiteSemigroup #-}
unTransiteSemigroup :: TransiteSemigroup a -> U.Vector (Int, a)
unTransiteSemigroup (TransiteSemigroup vec) = vec

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

-- | @Int@ as target
instance (U.Unbox a) => SemigroupAction (TransiteSemigroup a) Int where
  {-# INLINE sact #-}
  sact (TransiteSemigroup _) (-1) = -1
  sact (TransiteSemigroup vec) i =
    let (!i', !_) = vec U.! i
     in i'

-- | @b@ as target
instance (U.Unbox a, SemigroupAction a b) => SemigroupAction (TransiteSemigroup a) (Int, b) where
  {-# INLINE sact #-}
  sact (TransiteSemigroup _) (-1, !b) = (-1, b)
  sact (TransiteSemigroup vec) (!i, !b) =
    let (!i', !a) = vec U.! i
        !b' = a `sact` b
     in (i', b')

-- | The identity element of `TransiteSemigroup`.
{-# INLINE idTransiteSemigroup #-}
idTransiteSemigroup :: (U.Unbox a) => Int -> a -> TransiteSemigroup a
idTransiteSemigroup !n !ident = TransiteSemigroup $ U.generate n (,ident)

instance (U.Unbox a, Semigroup a) => BinaryLifting (TransiteSemigroup a) where
  type VecBL (TransiteSemigroup a) = V.Vector (TransiteSemigroup a)
  {-# INLINE cacheBL #-}
  cacheBL = cacheBLV
