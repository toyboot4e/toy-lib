-- | Strict segment tree
module Data.SegmentTree.Strict where

import Algorithm.Bisect
import Control.Monad (forM_, when)
import Control.Monad.Fix
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bits
import Data.Ix
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Stack (HasCallStack)
import ToyLib.Debug

-- | A mutable segment tree backed by a complete binary tree.
--
-- = Overview
--
-- A segment tree is a complete binary tree of monoid folding results.
-- Each vertex corresponds to a folding range and the result.
--
-- = Monoids
--
-- - `Sum` (`+`), `Product` (`*`)
-- - `Max` (`max`), `Min` (`min`)
-- - `Any` (`||`), `All` (`&&`)
--
-- = (Internal) 1-based indices
--
-- Use 1-based indices for super handy vertex indices:
--
-- @
--            1             |
--      2           3       | height = 4 = log_2 16
--   4     5     6     7    |
-- 08 09 10 11 12 13 14 15  v
-- ^
-- +-- nVerts / 2
--
-- 0  1  2  3  4  5  6  7   -- iLeaf is given by user and uses zero-based indices.
--
-- - parent = v .>>. 1
-- - childL = v .<<. 1
-- - childR = v .<<. 1 .|. 1
-- @
--
-- = Bottom-up vs top-down folding implementation
--
-- Most of the time, the top-down approach has to retrieve the values of the bottom leaves.
-- So bottom-up implementation is almost always faster.
data SegmentTree v s a = SegmentTree {unSegmentTree :: !(v s a), nValidLeavesSegmentTree :: !Int}

-- | \(O(\log N)\) Creates a segment tree for `n` leaves.
newSTree :: (U.Unbox a, Monoid a, PrimMonad m) => Int -> m (SegmentTree UM.MVector (PrimState m) a)
newSTree nValidLeaves = do
  vec <- GM.replicate nVerts mempty
  return $ SegmentTree vec nValidLeaves
  where
    !nVerts = until (>= (nValidLeaves .<<. 1)) (.<<. 1) (1 :: Int)

-- | \(O(\log N)\) Creates a segment tree from the given leaf values.
buildSTree :: (U.Unbox a, Monoid a, PrimMonad m) => U.Vector a -> m (SegmentTree UM.MVector (PrimState m) a)
buildSTree leaves = do
  verts <- GM.unsafeNew nVerts

  -- write leaf values
  G.unsafeCopy (GM.unsafeSlice nLeaves (G.length leaves) verts) leaves

  -- write node values
  forM_ [nLeaves - 1, nLeaves - 2 .. 1] $ \i -> do
    !x' <- (<>) <$> GM.unsafeRead verts (i .<<. 1) <*> GM.unsafeRead verts ((i .<<. 1) .|. 1)
    GM.unsafeWrite verts i x'

  return $ SegmentTree verts nValidLeaves
  where
    !nValidLeaves = G.length leaves
    !nVerts = until (>= (nValidLeaves .<<. 1)) (.<<. 1) (1 :: Int)
    !nLeaves = nVerts .>>. 1

-- | \(O(\log N)\) Reads a leaf value.
readSTree :: (HasCallStack, GM.MVector v a, PrimMonad m) => SegmentTree v (PrimState m) a -> Int -> m a
readSTree (SegmentTree vec nValidLeaves) i = GM.unsafeRead vec (nLeaves + i)
  where
    !_ = dbgAssert (inRange (0, nValidLeaves - 1) i) $ "readSTree: given invalid index: " ++ show i ++ " out of " ++ show nValidLeaves
    nLeaves = GM.length vec .>>. 1

-- | \(O(\log N)\) (Internal) Updates parent nodes after modifying a leaf.
_unsafeUpdateParentNodes :: (Monoid a, GM.MVector v a, PrimMonad m) => v (PrimState m) a -> Int -> m ()
_unsafeUpdateParentNodes vec v0 = do
  flip fix (v0 .>>. 1) $ \loop v -> do
    !x' <- (<>) <$> GM.unsafeRead vec (v .<<. 1) <*> GM.unsafeRead vec ((v .<<. 1) .|. 1)
    GM.unsafeWrite vec v x'
    when (v > 1) $ loop (v .>>. 1)

-- | \(O(\log N)\) Writes a leaf value.
writeSTree :: (HasCallStack, Monoid a, GM.MVector v a, PrimMonad m) => SegmentTree v (PrimState m) a -> Int -> a -> m ()
writeSTree (SegmentTree vec nValidLeaves) i x = do
  let v0 = nLeaves + i
  GM.unsafeWrite vec v0 x
  _unsafeUpdateParentNodes vec v0
  where
    !_ = dbgAssert (inRange (0, nValidLeaves - 1) i) $ "writeSTree: given invalid index: " ++ show i ++ " is out of " ++ show nValidLeaves
    nLeaves = GM.length vec .>>. 1

-- | \(O(\log N)\) Modifies a leaf value.
modifySTree :: (HasCallStack, Monoid a, GM.MVector v a, PrimMonad m) => SegmentTree v (PrimState m) a -> (a -> a) -> Int -> m ()
modifySTree (SegmentTree vec nValidLeaves) f i = do
  let v0 = nLeaves + i
  GM.unsafeModify vec f v0
  _unsafeUpdateParentNodes vec v0
  where
    !_ = dbgAssert (inRange (0, nValidLeaves - 1) i) $ "modifySTree: given invalid index: " ++ show i ++ " is out of " ++ show nValidLeaves
    nLeaves = GM.length vec .>>. 1

-- | \(O(\log N)\) Folds a non-empty @[l, r]@ span. Returns a broken avlue when given invalid range
-- (so this is @unsafezfoldSTree@).
foldSTree :: (HasCallStack, Monoid a, GM.MVector v a, PrimMonad m) => SegmentTree v (PrimState m) a -> Int -> Int -> m a
foldSTree (SegmentTree vec nValidLeaves) l0 r0 = glitchFold (l0 + nLeaves) (r0 + nLeaves) mempty mempty
  where
    !_ = dbgAssert (l0 <= r0 && inRange (0, nValidLeaves - 1) l0 && inRange (0, nValidLeaves - 1) r0) $ "foldSTree: given invalid range: " ++ show (l0, r0) ++ " is out of " ++ show nValidLeaves
    !nLeaves = GM.length vec .>>. 1
    glitchFold l r lx rx
      | l > r = return $! lx <> rx
      | otherwise = do
          !lx' <-
            if testBit l 0
              then (lx <>) <$> GM.unsafeRead vec l
              else return lx
          !rx' <-
            if not (testBit r 0)
              then (<> rx) <$> GM.unsafeRead vec r
              else return rx
          glitchFold ((l + 1) .>>. 1) ((r - 1) .>>. 1) lx' rx'

-- | \(O(\log N)\) Folds a non-empty @[l, r]@ span. Returns `Nothing` when given invalid range.
foldMaySTree :: (HasCallStack, Monoid a, GM.MVector v a, PrimMonad m) => SegmentTree v (PrimState m) a -> Int -> Int -> m (Maybe a)
foldMaySTree stree@(SegmentTree vec _) l0 r0
  | l0 > r0 || not (inRange (0, nLeaves - 1) l0) || not (inRange (0, nLeaves - 1) r0) = return Nothing
  | otherwise = Just <$> foldSTree stree l0 r0
  where
    nLeaves = GM.length vec .>>. 1

-- | \(O(1)\) Reads the whole span segment.
foldAllSTree :: (HasCallStack, GM.MVector v a, PrimMonad m) => SegmentTree v (PrimState m) a -> m a
foldAllSTree (SegmentTree vec _) = GM.read vec 1

-- TODO: faster bsearch

-- | \(O(\log^2 N)\)
bsearchSTree :: (HasCallStack, Monoid a, GM.MVector v a, PrimMonad m) => SegmentTree v (PrimState m) a -> Int -> Int -> (a -> Bool) -> m (Maybe Int, Maybe Int)
bsearchSTree stree@(SegmentTree _ nValidLeaves) l0 r0 f = do
  let !_ = dbgAssert (l0 <= r0 && inRange (0, nValidLeaves - 1) l0 && inRange (0, nValidLeaves - 1) l0) $ "bsearhSTree: wrong range " ++ show (l0, r0) ++ " for " ++ show nValidLeaves
  bisectM l0 r0 $ \r -> do
    x <- foldSTree stree l0 r
    return $ f x

-- | \(O(\log^2 N)\)
bsearchSTreeL :: (HasCallStack, Monoid a, GM.MVector v a, PrimMonad m) => SegmentTree v (PrimState m) a -> Int -> Int -> (a -> Bool) -> m (Maybe Int)
bsearchSTreeL stree l0 r0 f = fst <$> bsearchSTree stree l0 r0 f

-- | \(O(\log^2 N)\)
bsearchSTreeR :: (HasCallStack, Monoid a, GM.MVector v a, PrimMonad m) => SegmentTree v (PrimState m) a -> Int -> Int -> (a -> Bool) -> m (Maybe Int)
bsearchSTreeR stree l0 r0 f = snd <$> bsearchSTree stree l0 r0 f
