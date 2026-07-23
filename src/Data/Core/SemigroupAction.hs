{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Semigroup action @*@ is an operator where \(s_2 * (s_1 * a) == (s_2 \diamond s_1) * a\) holds.
module Data.Core.SemigroupAction where

import AtCoder.Extra.Math qualified as EM
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

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
  GT -> EM.stimes' n0 s0 `sact` a0

instance (Semigroup a) => SemigroupAction a a where
  {-# INLINE sact #-}
  sact = (<>)
