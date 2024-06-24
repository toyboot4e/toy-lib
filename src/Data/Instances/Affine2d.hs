{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | `Affine2d` represents @x -> a x + b@.
module Data.Instances.Affine2d where

import Data.Core.SemigroupAction
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- | `Affine2d` represents @x -> a x + b@.
type Affine2dRepr a = (a, a)

-- | Affine2d: x -> ax + b
instance (Num a) => Semigroup (Affine2d a) where
  {-# INLINE (<>) #-}
  (Affine2d (!a1, !b1)) <> (Affine2d (!a2, !b2)) = Affine2d (a2 * a1, a1 * b2 + b1)

instance (Num a) => Monoid (Affine2d a) where
  {-# INLINE mempty #-}
  mempty = Affine2d (1, 0)

instance (Num a) => SemigroupAction (Affine2d a) a where
  {-# INLINE sact #-}
  sact (Affine2d (!a, !b)) x = a * x + b

instance (Num a) => SemigroupAction (Affine2d a) (V1 a) where
  {-# INLINE sact #-}
  sact (Affine2d (!a, !b)) (V1 !x) = V1 (a * x + b)

newtype Affine2d a = Affine2d (Affine2dRepr a)
  deriving newtype (Eq, Ord, Show)

-- Unbox
newtype instance U.MVector s (Affine2d a) = MV_Affine2d (U.MVector s (Affine2dRepr a))

newtype instance U.Vector (Affine2d a) = V_Affine2d (U.Vector (Affine2dRepr a))

deriving instance (U.Unbox a) => GM.MVector UM.MVector (Affine2d a)

deriving instance (U.Unbox a) => G.Vector U.Vector (Affine2d a)

instance (U.Unbox a) => U.Unbox (Affine2d a)

-- | Affine2d transformation target.
newtype V1 a = V1 a
  deriving (P.Prim)
  deriving newtype (Eq, Ord, Show)

instance (Num a) => Semigroup (V1 a) where
  {-# INLINE (<>) #-}
  (V1 a) <> (V1 b) = V1 (a + b)

instance (Num a) => Monoid (V1 a) where
  {-# INLINE mempty #-}
  mempty = V1 0

-- Unbox
newtype instance U.MVector s (V1 a) = MV_V1 (U.MVector s a)

newtype instance U.Vector (V1 a) = V_V1 (U.Vector a)

deriving instance (U.Unbox a) => GM.MVector UM.MVector (V1 a)

deriving instance (U.Unbox a) => G.Vector U.Vector (V1 a)

instance (U.Unbox a) => U.Unbox (V1 a)
