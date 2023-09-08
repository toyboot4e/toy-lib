{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | `Int` with automatic moudlo arithmetic performed. Depends on @Math.PowMod@.
module Data.ModInt where

import Data.Coerce
import Data.Proxy
import qualified Data.Ratio as Ratio
import Data.Semigroup
import Data.SemigroupAction
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Base as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Math.PowMod (invModF)

-- | Type level constant `Int` value.
-- TODO: Replace with `GHC.TypaNats.KnownNat`: <https://zenn.dev/mod_poppo/books/haskell-type-level-programming/viewer/ghc-typenats>
class TypeInt a where
  typeInt :: Proxy a -> Int

-- | `Int` with automatic moudlo arithmetic performed.
newtype ModInt p = ModInt {toInt :: Int}
  deriving (Eq, VP.Prim)
  deriving newtype (Ord, Read, Show, Real)

instance (TypeInt p) => Num (ModInt p) where
  (ModInt !x1) + (ModInt !x2) = ModInt $! (x1 + x2) `mod` typeInt (Proxy @p)
  (ModInt !x1) * (ModInt !x2) = ModInt $! (x1 * x2) `mod` typeInt (Proxy @p)
  negate (ModInt !v) = ModInt $ (-v) `mod` typeInt (Proxy @p)
  abs = id
  signum _ = 1
  fromInteger = ModInt . fromInteger

-- FIXME: Use `KnownNat`
instance (TypeInt p) => Fractional (ModInt p) where
  -- \| Reciprocal of x (inverse of x).
  -- REMARK: This is TOO slow. Do cache when possible.
  recip (ModInt !x) = ModInt $! invModF x (typeInt (Proxy @p))
  fromRational !r = ModInt n / ModInt d
    where
      n = fromInteger $! Ratio.numerator r
      d = fromInteger $! Ratio.denominator r

instance (TypeInt p) => Enum (ModInt p) where
  toEnum = ModInt . (`mod` typeInt (Proxy @p))
  fromEnum = coerce

instance (TypeInt p) => SemigroupAction (Product (ModInt p)) (ModInt p) where
  sact (Product !x1) !x2 = x1 * x2

newtype instance VU.MVector s (ModInt p) = MV_ModInt (VP.MVector s (ModInt p))

newtype instance VU.Vector (ModInt p) = V_ModInt (VP.Vector (ModInt p))

deriving via (VU.UnboxViaPrim (ModInt p)) instance VGM.MVector VUM.MVector (ModInt p)

deriving via (VU.UnboxViaPrim (ModInt p)) instance VG.Vector VU.Vector (ModInt p)

instance VU.Unbox (ModInt p)
