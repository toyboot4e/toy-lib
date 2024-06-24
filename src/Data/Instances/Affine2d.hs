{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | `Affine2d` represents @x -> a x + b@. `V2` is the target type.
module Data.Instances.Affine2d where

import Data.Core.SemigroupAction
import Data.Instances.Mat2x2
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- | `Affine2d` represents @x -> a x + b@.
type Affine2dRepr a = (a, a)

-- | Affine2d: x -> ax + b
instance (Num a) => Semigroup (Affine2d a) where
  {-# INLINE (<>) #-}
  (Affine2d (!a1, !b1)) <> (Affine2d (!a2, !b2)) = Affine2d (a2 * a1, a1 * b2 + b1)

-- return Act(g.b * f.b, g.b * f.c + g.c);

instance (Num a) => Monoid (Affine2d a) where
  {-# INLINE mempty #-}
  mempty = Affine2d (1, 0)

instance (Num a) => SemigroupAction (Affine2d a) a where
  {-# INLINE sact #-}
  sact (Affine2d (!a, !b)) x = a * x + b

instance (Num a) => SemigroupAction (Affine2d a) (V2 a) where
  {-# INLINE sact #-}
  sact (Affine2d (!a, !b)) (V2 (!x, !len)) = V2 (a * x + b * len, len)

newtype Affine2d a = Affine2d (Affine2dRepr a)
  deriving newtype (Eq, Ord, Show)

{- ORMOLU_DISABLE -}
newtype instance U.MVector s (Affine2d a) = MV_Affine2d (U.MVector s (Affine2dRepr a))
newtype instance U.Vector (Affine2d a) = V_Affine2d (U.Vector (Affine2dRepr a))
deriving instance (U.Unbox a) => GM.MVector UM.MVector (Affine2d a)
deriving instance (U.Unbox a) => G.Vector U.Vector (Affine2d a)
instance (U.Unbox a) => U.Unbox (Affine2d a)
{- ORMOLU_ENABLE -}
