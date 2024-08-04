-- | Semigroup action @*@ is an operator where \(s_2 * (s_1 * a) == (s_2 ⊕ s_1) * a\) holds.
module Data.Core.SemigroupAction where

-- | Right semigroup aciton.
class SemigroupAction s a where
  -- | Right semigroup aciton
  sact :: s -> a -> a

-- | Target self
instance (Semigroup a) => SemigroupAction a a where
  {-# INLINE sact #-}
  sact x y = x <> y

