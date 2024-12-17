{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | `Affine1`, `Mat2x2` and `V2` as a `SemigroupAction` instance.
--
-- `Affine1` or `Mat2x2` represents \(f: x \rightarrow a x + b\). `V2` is the target vector type
-- with scaling information.
--
-- REMARK: It is super important to have @1@ as the second element in `V2`. Or else it fails to
-- calculate comopsitional affine transformation.
--
-- FIXME: It's Affine1, not Affine1
module Data.Instances.Affine1 where

import Data.Core.Group
import Data.Core.SegmentAction
import Data.Core.SemigroupAction
import Data.Semigroup
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- TODO: implement functor?

-- | 2D affine transformation \(f: x \rightarrow a x + b\)
--
-- The acted target type is `V2`, which holds the length at the second element.
--
-- = Composition and dual
--
-- \((f_1 \diamond f_2) v := (f_1 . f_2) v\). If yo need foldr [f_l, .., f_r] on segment tree, be
-- sure to wrap `Affine1` with `Dual`.
newtype Affine1 a = Affine1 (Affine1Repr a)
  deriving newtype (Eq, Ord, Show)

identAffine1 :: (Num a) => Affine1 a
identAffine1 = Affine1 (1, 0)

-- | `Affine1` represents @x -> a x + b@.
type Affine1Repr a = (a, a)

instance (Num a) => Semigroup (Affine1 a) where
  {-# INLINE (<>) #-}
  (Affine1 (!a1, !b1)) <> (Affine1 (!a2, !b2)) = Affine1 (a', b')
    where
      !a' = a1 * a2
      !b' = a1 * b2 + b1

instance (Num a) => Monoid (Affine1 a) where
  {-# INLINE mempty #-}
  mempty = identAffine1

-- * SegmentAction implementations

instance (Integral a) => SegmentAction (Affine1 a) a where
  {-# INLINE segActWithLength #-}
  segActWithLength !len (Affine1 (!a, !b)) !x = a'
    where
      !a' = a * x + b * fromIntegral len

instance (Integral a) => SegmentAction (Affine1 a) (Sum a) where
  {-# INLINE segActWithLength #-}
  segActWithLength !len (Affine1 (!a, !b)) (Sum !x) = Sum a'
    where
      !a' = a * x + b * fromIntegral len

instance (Integral a) => SegmentAction (Affine1 a) (Product a) where
  {-# INLINE segActWithLength #-}
  segActWithLength !len (Affine1 (!a, !b)) (Product !x) = Product a'
    where
      !a' = a * x + b * fromIntegral len

-- * SemigroupAction implementations

-- | Limited to length 1
instance (Num a) => SemigroupAction (Affine1 a) a where
  {-# INLINE sact #-}
  sact (Affine1 (!a, !b)) !x = x'
    where
      !x' = a * x + b

instance (Num a) => SemigroupAction (Affine1 a) (V2 a) where
  {-# INLINE sact #-}
  sact (Affine1 (!a, !b)) (V2 (!x, !len)) = V2 (a', len)
    where
      !a' = a * x + b * len

instance (Num a) => SegmentAction (Affine1 a) (V2 a) where
  {-# INLINE segActWithLength #-}
  segActWithLength _ = sact

-- | 2x2 unboxed matrix that works as a 2D affine transformation to `V2`. Prefer `Affine1` for
-- efficiency.
newtype Mat2x2 a = Mat2x2 (Mat2x2Repr a)
  deriving newtype (Eq, Ord, Show)

-- | `Mat2x2` internal unboxed data representaton.
type Mat2x2Repr a = (a, a, a, a)

{-# INLINE toMat2x2 #-}
toMat2x2 :: (Num a) => a -> a -> Mat2x2 a
toMat2x2 a b = Mat2x2 (a, b, 0, 1)

{-# INLINE unMat2x2 #-}
unMat2x2 :: Mat2x2 a -> (a, a, a, a)
unMat2x2 (Mat2x2 (!a, !b, !c, !d)) = (a, b, c, d)

instance (Num a) => Semigroup (Mat2x2 a) where
  {-# INLINE (<>) #-}
  (<>) = mulM22M22

{-# INLINE mapM22 #-}
mapM22 :: (a -> b) -> Mat2x2 a -> Mat2x2 b
mapM22 f (Mat2x2 (!a11, !a12, !a21, !a22)) = Mat2x2 (a11', a12', a21', a22')
  where
    !a11' = f a11
    !a12' = f a12
    !a21' = f a21
    !a22' = f a22

-- | Multiplies 2x2 matrix to a 2x2 matrix.
{-# INLINE mulM22M22 #-}
mulM22M22 :: (Num a) => Mat2x2 a -> Mat2x2 a -> Mat2x2 a
mulM22M22 (Mat2x2 (!a11, !a12, !a21, !a22)) (Mat2x2 (!b11, !b12, !b21, !b22)) = Mat2x2 (c11, c12, c21, c22)
  where
    !c11 = a11 * b11 + a12 * b21
    !c12 = a11 * b12 + a12 * b22
    !c21 = a21 * b11 + a22 * b21
    !c22 = a21 * b12 + a22 * b22

-- | \(O(N^2)\) Returns NxN unit matrix, based on `Group` and `Fractional`.
{-# INLINE invMat2x2 #-}
invMat2x2 :: (Fractional e) => Mat2x2 e -> Mat2x2 e
invMat2x2 (Mat2x2 (!a, !b, !c, !d)) = Mat2x2 (r * d, r * (-b), r * (-c), r * a)
  where
    -- {-# NOINLINE #-} -- works?
    !r = recip $ a * d - b * c

instance (Num a) => SemigroupAction (Mat2x2 a) (V2 a) where
  {-# INLINE sact #-}
  sact = mulM22V2

instance (Num a) => Monoid (Mat2x2 a) where
  -- \| Identity matrix or affine transformation as @x1@.
  {-# INLINE mempty #-}
  mempty = Mat2x2 (1, 0, 0, 1)

instance (Fractional e) => Group (Mat2x2 e) where
  {-# INLINE invert #-}
  invert = invMat2x2

-- | Multiplies 2x2 matrix to a 2 vector.
{-# INLINE mulM22V2 #-}
mulM22V2 :: (Num a) => Mat2x2 a -> V2 a -> V2 a
mulM22V2 (Mat2x2 (!a11, !a12, !a21, !a22)) (V2 (!x1, !x2)) = V2 (a', b')
  where
    !a' = a11 * x1 + a12 * x2
    !b' = a21 * x1 + a22 * x2

-- | Two-dimensional unboxed vector. Implements `Semigroup V2` as sum.
newtype V2 a = V2 (V2Repr a) deriving newtype (Eq, Ord, Show)

-- | `V2` internal unboxed data representaton. Be sure to have @1@ as the second element on
-- construction.
type V2Repr a = (a, a)

{-# INLINE toV2 #-}
toV2 :: (Num a) => a -> V2 a
toV2 a = V2 (a, 1)

{-# INLINE unV2 #-}
unV2 :: V2 a -> a
unV2 (V2 (!x, !_)) = x

{-# INLINE mapV2 #-}
mapV2 :: (a -> b) -> V2 a -> V2 b
mapV2 f (V2 (!a, !b)) = V2 (a', b')
  where
    !a' = f a
    !b' = f b

instance (Num a) => Semigroup (V2 a) where
  {-# INLINE (<>) #-}
  (V2 (!a1, !a2)) <> (V2 (!b1, !b2)) = V2 (a', b')
    where
      !a' = a1 + b1
      !b' = a2 + b2

instance (Num a) => Monoid (V2 a) where
  -- \| Identity element as @Summ@.
  {-# INLINE mempty #-}
  mempty = V2 (0, 0)

{- ORMOLU_DISABLE -}
newtype instance U.MVector s (Affine1 a) = MV_Affine1 (U.MVector s (Affine1Repr a))
newtype instance U.Vector (Affine1 a) = V_Affine1 (U.Vector (Affine1Repr a))
deriving instance (U.Unbox a) => GM.MVector UM.MVector (Affine1 a)
deriving instance (U.Unbox a) => G.Vector U.Vector (Affine1 a)
instance (U.Unbox a) => U.Unbox (Affine1 a)

newtype instance U.MVector s (V2 a) = MV_V2 (U.MVector s (V2Repr a))
newtype instance U.Vector (V2 a) = V_V2 (U.Vector (V2Repr a))
deriving instance (U.Unbox a) => GM.MVector UM.MVector (V2 a)
deriving instance (U.Unbox a) => G.Vector U.Vector (V2 a)
instance (U.Unbox a) => U.Unbox (V2 a)

newtype instance U.MVector s (Mat2x2 a) = MV_Mat2x2 (U.MVector s (Mat2x2Repr a))
newtype instance U.Vector (Mat2x2 a) = V_Mat2x2 (U.Vector (Mat2x2Repr a))
deriving instance (U.Unbox a) => GM.MVector UM.MVector (Mat2x2 a)
deriving instance (U.Unbox a) => G.Vector U.Vector (Mat2x2 a)
instance (U.Unbox a) => U.Unbox (Mat2x2 a)
{- ORMOLU_ENABLE -}
