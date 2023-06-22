{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | `Int` with automatic moudlo arithmetic performed. Depends on @Math.PowMod@.

module Data.ModInt where

import Data.Proxy
import qualified Data.Ratio as Ratio
import Data.Semigroup
import Data.SemigroupAction
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Math.PowMod (invModF)

-- {{{ ModInt

-- | Type level constant `Int` value.
-- TODO: Replace with `GHC.TypaNats.KnownNat`: <https://zenn.dev/mod_poppo/books/haskell-type-level-programming/viewer/ghc-typenats>
class TypeInt a where
  typeInt :: Proxy a -> Int

-- | `Int` with automatic moudlo arithmetic performed.
newtype ModInt p = ModInt {toInt :: Int}
  deriving (Eq)

derivingUnbox
  "ModInt"
  [t|forall p. ModInt p -> Int|]
  [|\(ModInt !x) -> x|]
  [|\ !x -> ModInt x|]

instance Show (ModInt p) where
  show = show . toInt

instance TypeInt p => Num (ModInt p) where
  (ModInt !x1) + (ModInt !x2) = ModInt $ (x1 + x2) `mod` typeInt (Proxy @p)
  (ModInt !x1) * (ModInt !x2) = ModInt $ (x1 * x2) `mod` typeInt (Proxy @p)
  negate (ModInt !v) = ModInt $ (-v) `mod` typeInt (Proxy @p)
  abs = id
  signum _ = 1
  fromInteger = ModInt . fromInteger

instance TypeInt p => Fractional (ModInt p) where
  -- reciprocal of x (inverse of x)
  recip (ModInt !x) = ModInt $ invModF x (typeInt (Proxy @p))
  fromRational !r = ModInt n / ModInt d
    where
      n = fromInteger $ Ratio.numerator r
      d = fromInteger $ Ratio.denominator r

instance TypeInt p => SemigroupAction (Product (ModInt p)) (ModInt p) where
  sact (Product !x1) !x2 = x1 * x2

-- }}}
