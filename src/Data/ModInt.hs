{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | `Int` with automatic moudlo arithmetic performed. Depends on @Math.PowMod@.
module Data.ModInt where

import Data.Coerce
import Data.Proxy
import qualified Data.Ratio as Ratio
import Data.Semigroup
import Data.SemigroupAction
import Data.Vector.Generic as VG
import Data.Vector.Generic.Mutable as VGM
import Data.Vector.Unboxed as VU
-- import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Data.Vector.Unboxed.Mutable as VUM
import Math.PowMod (invModF)

-- | Type level constant `Int` value.
-- TODO: Replace with `GHC.TypaNats.KnownNat`: <https://zenn.dev/mod_poppo/books/haskell-type-level-programming/viewer/ghc-typenats>
class TypeInt a where
  typeInt :: Proxy a -> Int

-- | `Int` with automatic moudlo arithmetic performed.
newtype ModInt p = ModInt {toInt :: Int}
  deriving (Eq)

instance Show (ModInt p) where
  show = show . toInt

instance TypeInt p => Num (ModInt p) where
  (ModInt !x1) + (ModInt !x2) = ModInt $! (x1 + x2) `mod` typeInt (Proxy @p)
  (ModInt !x1) * (ModInt !x2) = ModInt $! (x1 * x2) `mod` typeInt (Proxy @p)
  negate (ModInt !v) = ModInt $ (-v) `mod` typeInt (Proxy @p)
  abs = id
  signum _ = 1
  fromInteger = ModInt . fromInteger

instance TypeInt p => Fractional (ModInt p) where
  -- | Reciprocal of x (inverse of x).
  -- REMARK: This is TOO slow. Do cache when possible.
  recip (ModInt !x) = ModInt $! invModF x (typeInt (Proxy @p))
  fromRational !r = ModInt n / ModInt d
    where
      n = fromInteger $! Ratio.numerator r
      d = fromInteger $! Ratio.denominator r

instance (TypeInt p) => Enum (ModInt p) where
  toEnum = ModInt . (`mod` typeInt (Proxy @p))
  fromEnum = coerce

instance TypeInt p => SemigroupAction (Product (ModInt p)) (ModInt p) where
  sact (Product !x1) !x2 = x1 * x2

-- derivingUnbox
--   "ModInt"
--   [t|forall p. ModInt p -> Int|]
--   [|\(ModInt !x) -> x|]
--   [|\ !x -> ModInt x|]

newtype instance VUM.MVector s (ModInt p) = MV_ModInt (VUM.MVector s Int)

newtype instance VU.Vector (ModInt p) = V_ModInt (VU.Vector Int)

instance VU.Unbox (ModInt p)

instance VGM.MVector VUM.MVector (ModInt p) where
  basicLength (MV_ModInt v) = VGM.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (MV_ModInt v) = MV_ModInt $ VGM.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_ModInt v1) (MV_ModInt v2) = VGM.basicOverlaps v1 v2
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_ModInt <$> VGM.basicUnsafeNew n
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_ModInt v) = VGM.basicInitialize v
  {-# INLINE basicInitialize #-}
  basicUnsafeReplicate n x = MV_ModInt <$> VGM.basicUnsafeReplicate n (coerce x)
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV_ModInt v) i = coerce <$> VGM.basicUnsafeRead v i
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_ModInt v) i x = VGM.basicUnsafeWrite v i (coerce x)
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_ModInt v) = VGM.basicClear v
  {-# INLINE basicClear #-}
  basicSet (MV_ModInt v) x = VGM.basicSet v (coerce x)
  {-# INLINE basicSet #-}
  basicUnsafeCopy (MV_ModInt v1) (MV_ModInt v2) = VGM.basicUnsafeCopy v1 v2
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_ModInt v1) (MV_ModInt v2) = VGM.basicUnsafeMove v1 v2
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV_ModInt v) n = MV_ModInt <$> VGM.basicUnsafeGrow v n
  {-# INLINE basicUnsafeGrow #-}

instance VG.Vector VU.Vector (ModInt p) where
  basicUnsafeFreeze (MV_ModInt v) = V_ModInt <$> VG.basicUnsafeFreeze v
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_ModInt v) = MV_ModInt <$> VG.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_ModInt v) = VG.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (V_ModInt v) = V_ModInt $ VG.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_ModInt v) i = coerce <$> VG.basicUnsafeIndexM v i
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_ModInt mv) (V_ModInt v) = VG.basicUnsafeCopy mv v
  elemseq _ = seq
  {-# INLINE elemseq #-}
