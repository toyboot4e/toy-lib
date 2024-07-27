{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

-- | Type class-based binary lifting with caches. Prefer @Stimes@ module if it's one-shot.
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
  -- | @V.Vector a@ or @U.Vector a@
  type VecBL a
  -- | @cacheBLV@ or @cacheBLU@
  cacheBL :: a -> VecBL a

{-# INLINE cacheBLU #-}
cacheBLU :: (Semigroup a, U.Unbox a) => a -> U.Vector a
cacheBLU = U.iterateN 63 (\x -> x <> x)

{-# INLINE cacheBLV #-}
cacheBLV :: (Semigroup a) => a -> V.Vector a
cacheBLV = V.iterateN 63 (\x -> x <> x)

-- | \(O({(<>)} \cdot \mathit{popCount}(n))\)
{-# INLINE stimesBL #-}
stimesBL :: (Semigroup a, G.Vector v a) => v a -> Int -> a -> a
stimesBL cache n !s0 = U.foldl' step s0 (bitsOf n)
  where
    {-# INLINE step #-}
    step !s i = let !s' = s <> cache G.! i in s'

-- | \(O({(<>)} \cdot \mathit{popCount}(n))\)
{-# INLINE mtimesBL #-}
mtimesBL :: (Monoid a, G.Vector v a) => v a -> Int -> a
mtimesBL cache n = stimesBL cache n mempty

-- | \(O(\mathit{sact} \cdot \mathit{popCount}(n))\)
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
-- TransitionalSemigroup: Permutation + Semigroup
----------------------------------------------------------------------------------------------------

-- TODO: proper english

type Permutation = TransitionalSemigroup ()

-- | The identity element of `Permutation`.
{-# INLINE idPerm #-}
idPerm :: Int -> Permutation
idPerm = TransitionalSemigroup . (`U.generate` (,()))

-- | Transition + semigroup concatanation. LCA is based on this.
--
-- = Typical problems (Permutation)
-- - [Typical 058 - Original Calculator (★4)](https://atcoder.jp/contests/typical90/tasks/typical90_bf)
-- = Typical problems (Permutation)
--
-- = Typical problems (TransitionalSemigroup)
-- c.f. LCA.
newtype TransitionalSemigroup a = TransitionalSemigroup (U.Vector (Int, a))
  deriving (Show, Eq)

{-# INLINE unTransitionalSemigroup #-}
unTransitionalSemigroup :: TransitionalSemigroup a -> U.Vector (Int, a)
unTransitionalSemigroup (TransitionalSemigroup vec) = vec

instance (U.Unbox a, Semigroup a) => Semigroup (TransitionalSemigroup a) where
  {-# INLINE (<>) #-}
  TransitionalSemigroup r2 <> TransitionalSemigroup r1 = TransitionalSemigroup $ U.map f r1
    where
      {-# INLINE f #-}
      f (-1, !a) = (-1, a)
      f (!i, !a) =
        let (!i', !a') = r2 U.! i
            !a'' = a' <> a
         in (i', a'')

-- | @Int@ as target
instance (U.Unbox a) => SemigroupAction (TransitionalSemigroup a) Int where
  {-# INLINE sact #-}
  sact (TransitionalSemigroup _) (-1) = -1
  sact (TransitionalSemigroup vec) i =
    let (!i', !_) = vec U.! i
     in i'

-- | @b@ as target
instance (U.Unbox a, SemigroupAction a b) => SemigroupAction (TransitionalSemigroup a) (Int, b) where
  {-# INLINE sact #-}
  sact (TransitionalSemigroup _) (-1, !b) = (-1, b)
  sact (TransitionalSemigroup vec) (!i, !b) =
    let (!i', !a) = vec U.! i
        !b' = a `sact` b
     in (i', b')

-- | The identity element of `TransiteSemigroup`.
{-# INLINE idTransitionalSemigroup #-}
idTransitionalSemigroup :: (U.Unbox a) => Int -> a -> TransitionalSemigroup a
idTransitionalSemigroup !n !ident = TransitionalSemigroup $ U.generate n (,ident)

instance (U.Unbox a, Semigroup a) => BinaryLifting (TransitionalSemigroup a) where
  type VecBL (TransitionalSemigroup a) = V.Vector (TransitionalSemigroup a)
  {-# INLINE cacheBL #-}
  cacheBL = cacheBLV
