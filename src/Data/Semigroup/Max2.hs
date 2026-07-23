{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Semigroup.Max2 where

import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

newtype Max2 a = Max2 (Max2Repr a)
  deriving newtype (Eq, Ord, Show)

type Max2Repr a = (a, a)

instance (Ord a) => Semigroup (Max2 a) where
  {-# INLINE (<>) #-}
  (Max2 (!a1, !a2)) <> (Max2 (!b1, !b2)) = case compare a1 b1 of
    EQ -> Max2 (a1, b1)
    GT -> Max2 (a1, max a2 b1)
    LT -> Max2 (b1, max a1 b2)

instance (Ord a, Bounded a) => Monoid (Max2 a) where
  {-# INLINE mempty #-}
  mempty = Max2 (minBound, minBound)

newtype instance U.MVector s (Max2 a) = MV_Max2 (U.MVector s (Max2Repr a))

newtype instance U.Vector (Max2 a) = V_Max2 (U.Vector (Max2Repr a))

deriving instance (U.Unbox a) => GM.MVector UM.MVector (Max2 a)

deriving instance (U.Unbox a) => G.Vector U.Vector (Max2 a)

instance (U.Unbox a) => U.Unbox (Max2 a)
