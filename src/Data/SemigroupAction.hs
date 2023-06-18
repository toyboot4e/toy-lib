{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.SemigroupAction where

import Data.BinaryLifting
import Data.Bits
import Data.Monoid
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import ToyLib.Macro
import ToyLib.Prelude (rangeVG)

-- {{{ Semigroup action and binary lifting

-- | Right semigroup aciton.
--
-- s2 `sact` (s1 `sact` a) == (s2 <> s1) `sact` a
class SemigroupAction s a where
  -- | Right semigroup aciton
  sact :: s -> a -> a

-- | Binarily lifted semigroup action application.
sactBL :: (SemigroupAction s a, VG.Vector v s) => (BinaryLifting v s) -> a -> Int -> a
sactBL (BinaryLifting !ops) !acc0 !nAct = VU.foldl' step acc0 (rangeVG 0 62)
  where
    step !acc !nBit
      | testBit nAct nBit = (ops VG.! nBit) `sact` acc
      | otherwise = acc

-- | Right monoid action.
--
-- m2 `mact` (m1 `mact` a) == (m2 <> m1) `mact` a
class (SemigroupAction m a, Monoid m) => MonoidAction m a where
  -- | Right monoid aciton
  mact :: m -> a -> a
  mact = sact

instance SemigroupAction (Product Int) Int where
  sact (Product !x1) !x2 = x1 * x2

-- | Alias of `sactBL` for monoid action.
mactBL :: (MonoidAction m a, VG.Vector v m) => (BinaryLifting v m) -> a -> Int -> a
mactBL = sactBL

newtype Replacement = Replacement (VU.Vector Int)
  deriving (Show, Eq)

instance Semigroup Replacement where
  (Replacement vec1) <> (Replacement vec2) = Replacement $ VU.map (vec1 VU.!) vec2
    where
      !_ = dbgAssert (VG.length vec1 == VG.length vec2)

-- }}}
