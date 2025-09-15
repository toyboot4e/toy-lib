{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

-- | Type class-based binary lifting with caches. Prefer @Stimes@ module if it's one-shot.
-- Actually, `BinaryLifting` makes almost no sense.
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

-- | \(O(W(\diamond))\)
{-# INLINE cacheBLU #-}
cacheBLU :: (Semigroup a, U.Unbox a) => a -> U.Vector a
cacheBLU = U.iterateN 63 (\x -> x <> x)

-- | \(O(W(\diamond))\)
{-# INLINE cacheBLV #-}
cacheBLV :: (Semigroup a) => a -> V.Vector a
cacheBLV = V.iterateN 63 (\x -> x <> x)

-- | \(O((\diamond) \cdot \mathit{popCount}(n))\)
{-# INLINE stimesBL #-}
stimesBL :: (Semigroup a, G.Vector v a) => v a -> Int -> a -> a
stimesBL cache n !s0 = U.foldl' step s0 (bitsOf n)
  where
    {-# INLINE step #-}
    step !s i = let !s' = s <> cache G.! i in s'

-- | \(O({(\diamond)} \cdot \mathit{popCount}(n))\)
{-# INLINE mtimesBL #-}
mtimesBL :: (Monoid a, G.Vector v a) => v a -> Int -> a
mtimesBL cache n = stimesBL cache n mempty

-- | \(O(\mathit{sact} \cdot \mathit{popCount}(n))\) Performs semigroup action based on the binary
-- lifting table.
{-# INLINE sactBL #-}
sactBL :: (SemigroupAction a b, G.Vector v a) => v a -> Int -> b -> b
sactBL cache n !b0 = U.foldl' step b0 (bitsOf n)
  where
    {-# INLINE step #-}
    step !b i = let !b' = cache G.! i `sact` b in b'

-- * Product

instance SemigroupAction (Product Int) Int where
  {-# INLINE sact #-}
  sact (Product !x1) !x2 = x1 * x2

-- * Permutation

-- | Semigroup action of permutation.
newtype Permutation = Permutation (U.Vector Int)

-- | Creates identity `Permutation` of length `n`.
{-# INLINE idPermutation #-}
idPermutation :: Int -> Permutation
idPermutation = Permutation . (`U.generate` id)

-- | Creates identity `Permutation` of length `n`.
{-# INLINE unPermutation #-}
unPermutation :: Permutation -> U.Vector Int
unPermutation (Permutation x) = x

instance Semigroup Permutation where
  -- Because it's a permutation, it never fails.
  {-# INLINE (<>) #-}
  Permutation r2 <> Permutation r1 = Permutation $ U.backpermute r2 r1

-- | @Int@ as target
instance SemigroupAction Permutation Int where
  {-# INLINE sact #-}
  sact (Permutation vec) i = vec G.! i

-- | @U.Vector Int@ as target
instance SemigroupAction Permutation (U.Vector Int) where
  {-# INLINE sact #-}
  sact (Permutation indices) xs = U.backpermute xs indices

-- * IndexMap

-- | Semigroup action of @G.!@.
newtype IndexMap = IndexMap (U.Vector Int)

-- | Creates identity `Permutation` of length `n`.
{-# INLINE unIndexMap #-}
unIndexMap :: IndexMap -> U.Vector Int
unIndexMap (IndexMap x) = x

-- | Creates identity `Permutation` of length `n`.
{-# INLINE idIndexMap #-}
idIndexMap :: Int -> IndexMap
idIndexMap = IndexMap . (`U.generate` id)

instance Semigroup IndexMap where
  -- Because it's a permutation, it never fails.
  {-# INLINE (<>) #-}
  IndexMap r2 <> IndexMap r1 = IndexMap $ U.map f r1
    where
      {-# INLINE f #-}
      f (-1) = -1
      f (!i) = r2 G.! i

-- | @Int@ as target
instance SemigroupAction IndexMap Int where
  {-# INLINE sact #-}
  sact (IndexMap _) (-1) = -1
  sact (IndexMap vec) i = vec G.! i

-- * IndexMapWithAction: Transition + Semigroup

-- | Transition + semigroup concatanation. LCA is based on this.
--
-- = Typical problems (Permutation)
-- - [Typical 058 - Original Calculator (★4)](https://atcoder.jp/contests/typical90/tasks/typical90_bf)
-- = Typical problems (Permutation)
--
-- = Typical problems (IndexMapWithAction)
-- c.f. LCA.
newtype IndexMapWithAction a = IndexMapWithAction (U.Vector (Int, a))
  deriving (Show, Eq)

{-# INLINE unIndexMapWithAction #-}
unIndexMapWithAction :: IndexMapWithAction a -> U.Vector (Int, a)
unIndexMapWithAction (IndexMapWithAction vec) = vec

instance (U.Unbox a, Semigroup a) => Semigroup (IndexMapWithAction a) where
  {-# INLINE (<>) #-}
  IndexMapWithAction r2 <> IndexMapWithAction r1 = IndexMapWithAction $ U.map f r1
    where
      {-# INLINE f #-}
      f (-1, !a) = (-1, a)
      f (!i, !a) =
        let (!i', !a') = r2 G.! i
            !a'' = a' <> a
         in (i', a'')

-- | @Int@ as target
instance (U.Unbox a) => SemigroupAction (IndexMapWithAction a) Int where
  {-# INLINE sact #-}
  sact (IndexMapWithAction _) (-1) = -1
  sact (IndexMapWithAction vec) i =
    let (!i', !_) = vec G.! i
     in i'

-- | @b@ as target
instance (U.Unbox a, SemigroupAction a b) => SemigroupAction (IndexMapWithAction a) (Int, b) where
  {-# INLINE sact #-}
  sact (IndexMapWithAction _) (-1, !b) = (-1, b)
  sact (IndexMapWithAction vec) (!i, !b) =
    let (!i', !a) = vec G.! i
        !b' = a `sact` b
     in (i', b')

-- | The identity element of `IndexMapWithAction`.
{-# INLINE idIndexMapWithAction #-}
idIndexMapWithAction :: (U.Unbox a) => Int -> a -> IndexMapWithAction a
idIndexMapWithAction !n !ident = IndexMapWithAction $ U.generate n (,ident)
