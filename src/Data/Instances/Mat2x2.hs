{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | `Mat2x2` and `V2` is a `SemigroupAction` instance. Prefer @Affine2d@ for efficiency.
--
-- `Mat2x2` works as a 2D affine transformation when stored as @Mat2x2 (a, b, 0, 1)@.
--
-- REMARK: It is super important to have @1@ as the second element in `V2`. Or else it fails to
-- calculate comopsitional affine transformation.
module Data.Instances.Mat2x2 where

-- TODO: store in one array?

import Data.Core.SemigroupAction
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- | 2x2 unboxed matrix. Implements `SemigroupAction` to `V2`.
newtype Mat2x2 a = Mat2x2 (Mat2x2Repr a)
  deriving newtype (Eq, Ord, Show)

-- | `Mat2x2` internal unboxed data representaton.
type Mat2x2Repr a = (a, a, a, a)

{-# INLINE toMat2x2 #-}
toMat2x2 :: (Num a) => a -> a -> Mat2x2 a
toMat2x2 a b = Mat2x2 (a, b, 0, 1)

{-# INLINE unMat2x2 #-}
unMat2x2 :: Mat2x2 a -> (a, a)
unMat2x2 (Mat2x2 (!a, !b, !_, !_)) = (a, b)

instance (Num a) => Semigroup (Mat2x2 a) where
  {-# INLINE (<>) #-}
  (<>) = mulM22M22

-- | Multiplies 2x2 matrix to a 2x2 matrix.
{-# INLINE mulM22M22 #-}
mulM22M22 :: (Num a) => Mat2x2 a -> Mat2x2 a -> Mat2x2 a
mulM22M22 (Mat2x2 (!a11, !a12, !a21, !a22)) (Mat2x2 (!b11, !b12, !b21, !b22)) = Mat2x2 (c11, c12, c21, c22)
  where
    !c11 = a11 * b11 + a12 * b21
    !c12 = a11 * b12 + a12 * b22
    !c21 = a21 * b11 + a22 * b21
    !c22 = a21 * b12 + a22 * b22

instance (Num a) => Monoid (Mat2x2 a) where
  -- | Identity matrix or affine transformation as @x1@.
  {-# INLINE mempty #-}
  mempty = Mat2x2 (1, 0, 0, 1)

instance (Num a) => SemigroupAction (Mat2x2 a) (V2 a) where
  {-# INLINE sact #-}
  sact = mulM22V2

-- | Multiplies 2x2 matrix to a 2 vector.
{-# INLINE mulM22V2 #-}
mulM22V2 :: (Num a) => Mat2x2 a -> V2 a -> V2 a
mulM22V2 (Mat2x2 (!a11, !a12, !a21, !a22)) (V2 (!x1, !x2)) = V2 (a11 * x1 + a12 * x2, a21 * x1 + a22 * x2)

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

instance (Num a) => Semigroup (V2 a) where
  {-# INLINE (<>) #-}
  (V2 (!a1, !a2)) <> (V2 (!b1, !b2)) = V2 (a1 + b1, a2 + b2)

instance (Num a) => Monoid (V2 a) where
  -- | Identity element as @Summ@.
  {-# INLINE mempty #-}
  mempty = V2 (0, 0)

{- ORMOLU_DISABLE -}
newtype instance U.MVector s (Mat2x2 a) = MV_Mat2x2 (U.MVector s (Mat2x2Repr a))
newtype instance U.Vector (Mat2x2 a) = V_Mat2x2 (U.Vector (Mat2x2Repr a))
deriving instance (U.Unbox a) => GM.MVector UM.MVector (Mat2x2 a)
deriving instance (U.Unbox a) => G.Vector U.Vector (Mat2x2 a)
instance (U.Unbox a) => U.Unbox (Mat2x2 a)

newtype instance U.MVector s (V2 a) = MV_V2 (U.MVector s (V2Repr a))
newtype instance U.Vector (V2 a) = V_V2 (U.Vector (V2Repr a))
deriving instance (U.Unbox a) => GM.MVector UM.MVector (V2 a)
deriving instance (U.Unbox a) => G.Vector U.Vector (V2 a)
instance (U.Unbox a) => U.Unbox (V2 a)
{- ORMOLU_ENABLE -}
