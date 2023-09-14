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

-- | Semiring @(s, <+>, <.>)`. Sometimes called "deoid" (double monoid).
class Semiring s where
  -- | Plus (addition) opeator of the communicative monoid @(s, <+>, szero)@.
  (<+>) :: s -> s -> s
  -- | Identical element of the communicative monoid @(s, <+>, szero)@.
  szero :: s
  -- | Times (mulitplication) operator of the monoid @(s, <.>, sone)@.
  (<.>) :: s -> s -> s
  -- | Identical element of the monoid @(s, <.>, sone)@
  sone :: s

-- | Fold using the `<+>` (plus) opreator.
foldP :: (Semiring a, VG.Vector v a) => v a -> a
foldP = VG.foldl' (<+>) szero

-- | Fold using the `<.>` (times) opreator.
foldT :: (Semiring a, VG.Vector v a) => v a -> a
foldT = VG.foldl' (<.>) sone

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
  (MaxPlus x1) <+> (MaxPlus x2) = MaxPlus (x1 `max` x2)

  -- monoid with @<+>@ operator
  {-# INLINE szero #-}
  szero = MaxPlus minBound

  {-# INLINE (<.>) #-}
  (MaxPlus x1) <.> (MaxPlus x2) = MaxPlus (x1 + x2)

  -- monoid with @<.>@ operator
  {-# INLINE sone #-}
  sone = MaxPlus 0
