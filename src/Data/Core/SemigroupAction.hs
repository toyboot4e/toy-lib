{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Semigroup action @*@ is an operator where \(s_2 * (s_1 * a) == (s_2 \diamond s_1) * a\) holds.
module Data.Core.SemigroupAction where

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Math.Stimes (stimes')

-- | Right semigroup aciton.
class SemigroupAction s a where
  -- | Right semigroup aciton
  sact :: s -> a -> a

-- | Performs `sact` with the binary lifting technique. Prefer the binary lifting module if the
-- cache takes effect.
sactTimes :: (Semigroup s, SemigroupAction s a) => Int -> s -> a -> a
sactTimes n0 s0 a0 = case compare n0 0 of
  LT -> errorWithoutStackTrace "sactTimes: zero or positive multiplier expected"
  EQ -> a0
  GT -> stimes' n0 s0 `sact` a0

instance (Semigroup a) => SemigroupAction a a where
  {-# INLINE sact #-}
  sact = (<>)

