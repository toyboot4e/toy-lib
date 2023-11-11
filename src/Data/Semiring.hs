{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | `Semiring`.
module Data.Semiring where

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Base as U
import qualified Data.Vector.Unboxed.Mutable as UM

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
foldP :: (Semiring a, G.Vector v a) => v a -> a
foldP = G.foldl' (<+>) szero

-- | Fold using the `<.>` (times) opreator.
foldT :: (Semiring a, G.Vector v a) => v a -> a
foldT = G.foldl' (<.>) sone

-- | Max-Plus semiring
newtype MaxPlus a = MaxPlus {getMaxPlus :: a}
  deriving (P.Prim)
  deriving newtype (Eq, Ord, Show)

newtype instance U.MVector s (MaxPlus a) = MV_MaxPlus (P.MVector s (MaxPlus a))

newtype instance U.Vector (MaxPlus a) = V_MaxPlus (P.Vector (MaxPlus a))

deriving via (U.UnboxViaPrim (MaxPlus a)) instance (P.Prim a) => GM.MVector UM.MVector (MaxPlus a)

deriving via (U.UnboxViaPrim (MaxPlus a)) instance (P.Prim a) => G.Vector U.Vector (MaxPlus a)

instance (P.Prim a) => U.Unbox (MaxPlus a)

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
  deriving (P.Prim)
  deriving newtype (Eq, Ord, Show)

newtype instance U.MVector s (MinPlus a) = MV_MinPlus (P.MVector s (MinPlus a))

newtype instance U.Vector (MinPlus a) = V_MinPlus (P.Vector (MinPlus a))

deriving via (U.UnboxViaPrim (MinPlus a)) instance (P.Prim a) => GM.MVector UM.MVector (MinPlus a)

deriving via (U.UnboxViaPrim (MinPlus a)) instance (P.Prim a) => G.Vector U.Vector (MinPlus a)

instance (P.Prim a) => U.Unbox (MinPlus a)

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
  -- deriving (P.Prim)
  deriving newtype (Eq, Ord, Show)

-- NOTE: For some reason, the `primitive` package does not implement `Prim` for `Bool`.
-- I guess that's why `bitvec` exists: <https://github.com/Bodigrim/bitvec>
--
-- Here's some indirection to get `Unbox` implementation..
instance U.IsoUnbox Boolean Bool where
  {-# INLINE toURepr #-}
  toURepr (Boolean b) = b
  {-# INLINE fromURepr #-}
  fromURepr = Boolean

newtype instance U.MVector s Boolean = MV_Foo (U.MVector s Bool)

newtype instance U.Vector Boolean = V_Foo (U.Vector Bool)

deriving via (Boolean `U.As` Bool) instance GM.MVector UM.MVector Boolean

deriving via (Boolean `U.As` Bool) instance G.Vector U.Vector Boolean

instance U.Unbox Boolean

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
