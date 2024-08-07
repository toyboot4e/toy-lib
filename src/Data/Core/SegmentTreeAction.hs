-- | Right semigroup action, specialized for the lazily propagated segment tree.
--
-- = Algebra
-- \(s_2 * (s_1 * a) == (s_2 \diamond s_1) * a\)
module Data.Core.SegmentTreeAction where

-- | Right semigroup action, specialized for the lazily propagated segment tree.
class SegmentTreeAction op a where
  -- | Right semigroup aciton, limited to the lazily propagated segment tree.
  {-# INLINE segAct #-}
  segAct :: op -> a -> a
  segAct op a = segActWithLength op a 1

  -- | Right semigroup aciton with the segment length.
  segActWithLength :: op -> a -> Int -> a

-- TODO: consider making it failsible for beats.

-- | Target self
instance (Semigroup a) => SegmentTreeAction a a where
  {-# INLINE segActWithLength #-}
  segActWithLength x y _ = x <> y
