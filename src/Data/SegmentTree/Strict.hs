-- | Strict segment tree
module Data.SegmentTree.Strict where

import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Stack (HasCallStack)

-- {{{ Segment tree

-- TODO: refactor
-- TODO: use one-based indices internally?

-- | A mutable segment tree backed by a complete binary tree.
--
-- = Overview
--
-- A segment tree is a cache of a folding function.
-- Each node corresponds to a folding range and the node contains the folding result.
--
-- A segment tree has a constant size and never be resized.
--
-- = Operations
--
-- Modification takes \(O(log N)\), so creation takes \(N(log N)\).
-- Lookup takes \(O(log N)\).
--
-- = (Internal) Indices
--
-- The complete binary tree has @2 ^ depth - 1@ elements.
--
-- - Child elements of a parent node @i@ has index @2 * i + 1@ and @2 * i + 2@.
-- - The leaf indices start with @length / 2 - 1@.
--
-- Example:
--
-- @
--            0
--      1           2
--   3     4     5     6
-- 07 08 09 10 11 12 13 14
-- @
data SegmentTree v s a = SegmentTree (a -> a -> a) (v s a)

-- TODO: Can I UNPACK? the funciton?
-- TODO: Possibly a show instance?
-- TODO: Use 1-based index

-- | Creates a new segment tree for `n` leaves.
-- REMARK: Always give a zero value. It fills all the nodes including parent nodes, and the parent
-- nodes are not updated.
{-# INLINE newSTreeG #-}
newSTreeG :: (GM.MVector v a, PrimMonad m) => (a -> a -> a) -> Int -> a -> m (SegmentTree v (PrimState m) a)
newSTreeG !f !nLeaves !zero = SegmentTree f <$> GM.replicate nVerts zero
  where
    !nVerts = until (>= 2 * nLeaves) (* 2) 2

-- !nVerts = fromJust $ find ((>= (2 * nLeaves)) . bit) [0 .. 63]

-- | Creates a boxed segment tree.
{-# INLINE newSTreeV #-}
newSTreeV :: (PrimMonad m) => (a -> a -> a) -> Int -> a -> m (SegmentTree VM.MVector (PrimState m) a)
newSTreeV = newSTreeG

-- | Creates an unboxed segment tree.
{-# INLINE newSTreeU #-}
newSTreeU :: (U.Unbox a, PrimMonad m) => (a -> a -> a) -> Int -> a -> m (SegmentTree UM.MVector (PrimState m) a)
newSTreeU = newSTreeG

-- | Sets all the internal values of a segment tree to the given value which has to be zero.
--
-- REMARK: It takes lots of time. Consider a much more efficient resettiong strategy such as
-- re-inserting zeros to used slots, or maybe use | `compressInvNumG` when you just need
-- inversion number.
resetSTree :: (GM.MVector v a, PrimMonad m) => SegmentTree v (PrimState m) a -> a -> m ()
resetSTree (SegmentTree !_ !vec) !zero = GM.set vec zero

-- | Updates an `SegmentTree` leaf value and their parents up to top root.
{-# INLINE insertSTree #-}
insertSTree :: (GM.MVector v a, PrimMonad m) => SegmentTree v (PrimState m) a -> Int -> a -> m ()
insertSTree tree@(SegmentTree !_ !vec) !i !value = _updateElement tree i' value
  where
    -- length == 2 * (the number of the leaves)
    !offset = GM.length vec `div` 2 - 1
    -- leaf index
    !i' = i + offset

-- | Updates an `SegmentTree` leaf value and their parents up to top root.
{-# INLINE modifySTree #-}
modifySTree :: (GM.MVector v a, PrimMonad m) => SegmentTree v (PrimState m) a -> (a -> a) -> Int -> m ()
modifySTree tree@(SegmentTree !_ !vec) !f !i = do
  !v <- f <$> GM.unsafeRead vec i'
  _updateElement tree i' v
  where
    -- length == 2 * (the number of the leaves)
    !offset = GM.length vec `div` 2 - 1
    -- leaf index
    !i' = i + offset

-- | (Internal) Updates an `SegmentTree` element (node or leaf) value and their parents up to top root.
-- REMARK: It's faster to not INLINE the recursive function:
_updateElement :: (HasCallStack, GM.MVector v a, PrimMonad m) => SegmentTree v (PrimState m) a -> Int -> a -> m ()
_updateElement (SegmentTree !_ !vec) 0 !value = do
  GM.unsafeWrite vec 0 value
_updateElement tree@(SegmentTree !f !vec) !i !value = do
  GM.unsafeWrite vec i value
  case (i - 1) `div` 2 of
    -- REMARK: (-1) `div` 2 == -1
    -- TODO: This case never happens, right?
    (-1) -> return ()
    !iParent -> do
      !c1 <- GM.unsafeRead vec $! iParent * 2 + 1
      !c2 <- GM.unsafeRead vec $! iParent * 2 + 2
      _updateElement tree iParent $! f c1 c2

-- | Retrieves the folding result over the inclusive range `[l, r]` from `SegmentTree`.
{-# INLINE querySTree #-}
querySTree :: forall v a m. (HasCallStack, GM.MVector v a, PrimMonad m) => SegmentTree v (PrimState m) a -> (Int, Int) -> m (Maybe a)
querySTree (SegmentTree !f !vec) (!lo, !hi)
  | lo > hi = return Nothing
  | lo < 0 || hi >= (GM.length vec `div` 2) = return Nothing
  | otherwise = inner 0 (0, initialHi)
  where
    !initialHi = GM.length vec `div` 2 - 1
    inner :: Int -> (Int, Int) -> m (Maybe a)
    inner !i (!l, !h)
      | lo <= l && h <= hi = Just <$> GM.unsafeRead vec i
      | h < lo || hi < l = return Nothing
      | otherwise = do
          let !d = (h - l) `div` 2
          !ansL <- inner (2 * i + 1) (l, l + d)
          !ansH <- inner (2 * i + 2) (l + d + 1, h)
          pure . Just $ case (ansL, ansH) of
            (Just !a, Just !b) -> f a b
            (Just !a, _) -> a
            (_, Just !b) -> b
            (_, _) -> error $ "query error (segment tree): " ++ show (i, (l, h), (lo, hi))

-- }}}
