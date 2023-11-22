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
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Math.PowMod (invModF)

-- | Type level constant `Int` value.
-- TODO: Replace with `GHC.TypaNats.KnownNat`: <https://zenn.dev/mod_poppo/books/haskell-type-level-programming/viewer/ghc-typenats>
class TypeInt a where
  typeInt :: Proxy a -> Int

-- | `Int` with automatic moudlo arithmetic performed.
newtype ModInt p = ModInt {getModInt :: Int}
  deriving (Eq, P.Prim)
  deriving newtype (Ord, Read, Show, Real)

instance (TypeInt p) => Num (ModInt p) where
  (ModInt !x1) + (ModInt !x2) = ModInt $! (x1 + x2) `mod` typeInt (Proxy @p)
  (ModInt !x1) * (ModInt !x2) = ModInt $! (x1 * x2) `mod` typeInt (Proxy @p)
  negate (ModInt !v) = ModInt $ (-v) `mod` typeInt (Proxy @p)
  abs = id
  signum _ = 1
  fromInteger = ModInt . fromInteger

-- FIXME: Use @KnownNat@
-- TODO: prefer @recip@?
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

newtype instance U.MVector s (ModInt p) = MV_ModInt (P.MVector s (ModInt p))

newtype instance U.Vector (ModInt p) = V_ModInt (P.Vector (ModInt p))

deriving via (U.UnboxViaPrim (ModInt p)) instance GM.MVector UM.MVector (ModInt p)

deriving via (U.UnboxViaPrim (ModInt p)) instance G.Vector U.Vector (ModInt p)

instance U.Unbox (ModInt p)
