-- | Right semigroup action, specialized for the lazily propagated segment tree.
--
-- = Algebra
-- \(s_2 * (s_1 * a) == (s_2 \diamond s_1) * a\)
module Data.Core.SegmentAction where

-- | Right semigroup action, specialized for the lazily propagated segment tree.
class SegmentAction op a where
  -- | Right semigroup aciton, limited to the lazily propagated segment tree.
  {-# INLINE segAct #-}
  segAct :: op -> a -> a
  segAct op a = segActWithLength op a 1

  -- | Right semigroup aciton with the segment length.
  {-# INLINE segActWithLength #-}
  segActWithLength :: op -> a -> Int -> a
  segActWithLength op a _  = segAct op a

-- TODO: consider making it failsible for beats.

-- | Target self
instance (Semigroup a) => SegmentAction a a where
  {-# INLINE segActWithLength #-}
  segActWithLength x y _ = x <> y

