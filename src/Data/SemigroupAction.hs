{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Semigroup action @*@ is an operator where \(s_2 * (s_1 * a) == (s_2 ⊕ s_1) * a\) holds.

module Data.SemigroupAction where

import Data.Bits
import Data.Monoid
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import ToyLib.Macro

-- {{{ Semigroup action and binary lifting

-- | Right semigroup aciton.
class SemigroupAction s a where
  -- | Right semigroup aciton
  sact :: s -> a -> a

-- | Right monoid action.
class (SemigroupAction m a, Monoid m) => MonoidAction m a where
  -- | Right monoid aciton
  mact :: m -> a -> a
  mact = sact

instance SemigroupAction (Product Int) Int where
  sact (Product !x1) !x2 = x1 * x2

newtype Replacement = Replacement (VU.Vector Int)
  deriving (Show, Eq)

instance Semigroup Replacement where
  (Replacement vec1) <> (Replacement vec2) = Replacement $ VU.map (vec1 VU.!) vec2
    where
      !_ = dbgAssert (VG.length vec1 == VG.length vec2)

-- }}}
