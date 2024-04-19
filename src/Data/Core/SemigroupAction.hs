-- | Semigroup action @*@ is an operator where \(s_2 * (s_1 * a) == (s_2 ⊕ s_1) * a\) holds.
module Data.Core.SemigroupAction where

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
  {-# INLINE mact #-}
  mact = sact

-- | Target self
instance (Monoid a) => MonoidAction a a where
  mact = sact

