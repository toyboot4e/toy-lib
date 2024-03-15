-- | Semigroup action @*@ is an operator where \(s_2 * (s_1 * a) == (s_2 ⊕ s_1) * a\) holds.
module Data.Core.SemigroupAction where

import Data.Monoid
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import ToyLib.Debug

-- {{{ Semigroup action and binary lifting

-- | Right semigroup aciton.
class SemigroupAction s a where
  -- | Right semigroup aciton
  sact :: s -> a -> a

-- | Target self
instance (Semigroup a) => SemigroupAction a a where
  sact x y = x <> y

-- | Right monoid action.
class (SemigroupAction m a, Monoid m) => MonoidAction m a where
  -- | Right monoid aciton
  mact :: m -> a -> a
  mact = sact

instance SemigroupAction (Product Int) Int where
  sact (Product !x1) !x2 = x1 * x2

-- | Permutation of N sequence.
--
-- = Typical problems
-- - [Typical 058 - Original Calculator (★4)](https://atcoder.jp/contests/typical90/tasks/typical90_bf)
newtype Permutation = Permutation (U.Vector Int)
  deriving (Show, Eq)

instance Semigroup Permutation where
  (Permutation vec1) <> (Permutation vec2) = Permutation $! U.map (vec1 U.!) vec2
    where
      !_ = dbgAssert (G.length vec1 == G.length vec2)

instance SemigroupAction Permutation Int where
  sact (Permutation !vec) !i = vec U.! i

-- }}}
