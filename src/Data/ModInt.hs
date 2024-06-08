{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | `Int` with automatic moudlo arithmetic performed. Depends on @Math.PowMod@.
module Data.ModInt where

import qualified Data.ByteString.Builder as BSB
import Data.Coerce
import Data.Core.SemigroupAction
import Data.Ratio
import Data.Semigroup
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Exts (proxy#)
import GHC.TypeLits
import Math.PowMod (invModF)
import ToyLib.ShowBSB

-- | `Int` with automatic moudlo arithmetic performed.
newtype ModInt p = ModInt {unModInt :: Int}
  deriving (Eq, P.Prim)
  deriving newtype (Ord, Read, Show)

instance ShowBSB (ModInt p) where
  showBSB = BSB.intDec . unModInt

deriving newtype instance (KnownNat p) => Real (ModInt p)

instance (KnownNat p) => Num (ModInt p) where
  (ModInt !x1) + (ModInt !x2) = ModInt $! (x1 + x2) `mod` fromInteger (natVal' (proxy# @p))
  (ModInt !x1) * (ModInt !x2) = ModInt $! (x1 * x2) `mod` fromInteger (natVal' (proxy# @p))
  negate (ModInt !v) = ModInt $ (-v) `mod` fromInteger (natVal' (proxy# @p))
  abs = id
  signum _ = 1
  fromInteger = ModInt . fromInteger

instance (KnownNat p) => Fractional (ModInt p) where
  -- Reciprocal of x (inverse of x).
  -- FIXME: use exgcd instead.
  recip (ModInt !x) = ModInt $! invModF (fromInteger (natVal' (proxy# @p))) x
  fromRational !r = ModInt n / ModInt d
    where
      -- Data.Ratio
      n = fromInteger $! numerator r
      d = fromInteger $! denominator r

instance (KnownNat p) => Enum (ModInt p) where
  toEnum = ModInt . (`mod` fromInteger (natVal' (proxy# @p)))
  fromEnum = coerce

instance (KnownNat p) => SemigroupAction (Product (ModInt p)) (ModInt p) where
  sact (Product !x1) !x2 = x1 * x2

newtype instance U.MVector s (ModInt p) = MV_ModInt (P.MVector s (ModInt p))

newtype instance U.Vector (ModInt p) = V_ModInt (P.Vector (ModInt p))

deriving via (U.UnboxViaPrim (ModInt p)) instance GM.MVector UM.MVector (ModInt p)

deriving via (U.UnboxViaPrim (ModInt p)) instance G.Vector U.Vector (ModInt p)

instance U.Unbox (ModInt p)
