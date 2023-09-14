{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | `Semiring`
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

-- | Max-Plus semiring
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

-- | Min-Plus semiring
newtype MinPlus a = MinPlus {getMinPlus :: a}
  deriving (VP.Prim)
  deriving newtype (Eq, Ord, Show)

newtype instance VU.MVector s (MinPlus a) = MV_MinPlus (VP.MVector s (MinPlus a))

newtype instance VU.Vector (MinPlus a) = V_MinPlus (VP.Vector (MinPlus a))

deriving via (VU.UnboxViaPrim (MinPlus a)) instance (VP.Prim a) => VGM.MVector VUM.MVector (MinPlus a)

deriving via (VU.UnboxViaPrim (MinPlus a)) instance (VP.Prim a) => VG.Vector VU.Vector (MinPlus a)

instance (VP.Prim a) => VU.Unbox (MinPlus a)

instance (Num a, Bounded a, Ord a) => Semiring (MinPlus a) where
  {-# INLINE (<+>) #-}
  (MinPlus x1) <+> (MinPlus x2) = MinPlus (x1 `min` x2)

  -- monoid with @<+>@ operator
  {-# INLINE szero #-}
  szero = MinPlus maxBound

  {-# INLINE (<.>) #-}
  (MinPlus x1) <.> (MinPlus x2) = MinPlus (x1 + x2)

  -- monoid with @<.>@ operator
  {-# INLINE sone #-}
  sone = MinPlus 0

-- | Boolean semiring
newtype Boolean = Boolean {getBoolean :: Bool}
  -- deriving (VP.Prim)
  deriving newtype (Eq, Ord, Show)

-- NOTE: For some reason, the `primitive` package does not implement `Prim` for `Bool`.
-- I guess that's why `bitvec` exists: <https://github.com/Bodigrim/bitvec>
--
-- Here's some indirection to get `Unbox` implementation..
instance VU.IsoUnbox Boolean Bool where
  {-# INLINE toURepr #-}
  toURepr (Boolean b) = b
  {-# INLINE fromURepr #-}
  fromURepr = Boolean

newtype instance VU.MVector s Boolean = MV_Foo (VU.MVector s Bool)

newtype instance VU.Vector Boolean = V_Foo (VU.Vector Bool)

deriving via (Boolean `VU.As` Bool) instance VGM.MVector VUM.MVector Boolean

deriving via (Boolean `VU.As` Bool) instance VG.Vector VU.Vector Boolean

instance VU.Unbox Boolean

instance Semiring Boolean where
  {-# INLINE (<+>) #-}
  (Boolean x1) <+> (Boolean x2) = Boolean (x1 || x2)

  -- monoid with @<+>@ operator
  {-# INLINE szero #-}
  szero = Boolean False

  {-# INLINE (<.>) #-}
  (Boolean x1) <.> (Boolean x2) = Boolean (x1 && x2)

  -- monoid with @<.>@ operator
  {-# INLINE sone #-}
  sone = Boolean True
