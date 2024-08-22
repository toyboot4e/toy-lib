-- | Right semigroup action, specialized for the lazily propagated segment tree.
--
-- = Algebra
-- \(s_2 * (s_1 * a) == (s_2 \diamond s_1) * a\)
module Data.Core.SegmentAction where

import Data.Semigroup

-- FIXME: Int -> op -> a -z. a

-- | Right semigroup action, specialized for the lazily propagated segment tree.
class SegmentAction op a where
  -- | Right semigroup aciton, limited to the lazily propagated segment tree.
  {-# INLINE segAct #-}
  segAct :: op -> a -> a
  segAct = segActWithLength 1

  -- | Right semigroup aciton with the segment length.
  {-# INLINE segActWithLength #-}
  segActWithLength :: Int -> op -> a -> a
  segActWithLength _  = segAct

-- TODO: consider making it failsible for beats.

-- -- | Target self
-- instance (Semigroup a) => SegmentAction a a where
--   {-# INLINE segActWithLength #-}
--   segActWithLength x y _ = x <> y

-- TODO: more instances

instance (Integral a) => SegmentAction (Sum a) (Sum a) where
  {-# INLINE segActWithLength #-}
  segActWithLength len (Sum x) y = Sum (fromIntegral len * x) <> y

