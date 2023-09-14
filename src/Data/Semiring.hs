-- | `Semiring`
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Semiring where

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Base as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

class Semiring s where
  -- | Communicative monoid @(s, <+>, szero)@
  (<+>) :: s -> s -> s

  -- | Identical element of the communicative monoid @(s, <+>, szero)@
  szero :: s

  -- | Monoid @(s, <.>, sone)@
  (<.>) :: s -> s -> s

  -- | Identical element of the monoid @(s, <.>, sone)@
  sone :: s

newtype MaxPlus a = MaxPlus {getMaxPlus :: a}
  deriving (VP.Prim)
  deriving newtype (Eq, Ord, Show)

newtype instance VU.MVector s (MaxPlus a) = MV_MaxPlus (VP.MVector s (MaxPlus a))

newtype instance VU.Vector (MaxPlus a) = V_MaxPlus (VP.Vector (MaxPlus a))

deriving via (VU.UnboxViaPrim (MaxPlus a)) instance (VP.Prim a) => VGM.MVector VUM.MVector (MaxPlus a)

deriving via (VU.UnboxViaPrim (MaxPlus a)) instance (VP.Prim a) => VG.Vector VU.Vector (MaxPlus a)

instance (VP.Prim a) => VU.Unbox (MaxPlus a)

instance (Num a, Bounded a, Ord a) => Semiring (MaxPlus a) where
  {-# INLINE (<+>) #-}
  (MaxPlus x1) <+> (MaxPlus x2) = MaxPlus (x1 + x2)

  {-# INLINE (<.>) #-}
  (MaxPlus x1) <.> (MaxPlus x2) = MaxPlus (x1 `max` x2)

  -- monoid with @+@ operator
  {-# INLINE szero #-}
  szero = MaxPlus 0

  -- monoid with @max@ operator
  {-# INLINE sone #-}
  sone = MaxPlus minBound
