-- | Lazy segment tree, where we can perform operation over range.
--
-- = Typical problems
-- - [Typical 029 - Long Bricks (★5)](https://atcoder.jp/contests/typical90/tasks/typical90_ac)
-- - [EDPC W - Intervals](https://atcoder.jp/contests/dp/tasks/dp_w)
--
-- TODO: Add `set` / `get`, as in [ac-library](https://github.com/atcoder/ac-library/blob/master/atcoder/lazysegtree.hpp)
module Data.SegmentTree.Lazy where

import Control.Monad
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bifunctor
import Data.Bits
import Data.SemigroupAction
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import GHC.Stack (HasCallStack)
import ToyLib.Macro
import ToyLib.Prelude (forMS_, rangeMS, rangeMSR)

-- {{{ Lazy segment tree

-- TODO: Do we need to duplicate `SegmentTree` and `LazySegmentTree`?
-- TODO: Use generic vector type.. or not.
-- TODO: We're assuming commutative operator monoid in LazySegmentTree, right?
-- TODO: Vertex -> Node

-- | Lazy segment tree.
--
-- = Indices
--
-- Use 1-based indices for super handy index hacks:
--
-- @
--            1             |
--      2           3       | height = 4
--   4     5     6     7    |
-- 08 09 10 11 12 13 14 15  v
-- ^
-- +-- nVerts / 2
--
-- 0  1  2  3  4  5  6  7   -- iLeaf is given by user and uses zero-based indices.
--
-- - parentVertex = vertex / 2 = shiftR vertex 1
-- - leftChild = vertex * 2 = shiftR vertex 1
-- - rightChild = vertex * 2 + 1 = shiftR vertex 1 + 1 = (shiftR vertex 1) .|. 1
-- @
--
-- = Invariant
--
-- - New operators always come from right: @oldOp <> newOp@
data LazySegmentTree v a op s = LazySegmentTree !(v s a) !(VUM.MVector s op) !Int

-- | Creates `LazySegmentTree` with `mempty` as the initial accumulated values.
newLazySTree ::
  forall v a op m.
  (VGM.MVector v a, Monoid a, MonoidAction op a, VU.Unbox op, PrimMonad m) =>
  Int ->
  m (LazySegmentTree v a op (PrimState m))
newLazySTree !n = do
  !as <- VGM.replicate n2 mempty
  !ops <- VUM.replicate n2 mempty
  return $ LazySegmentTree as ops h
  where
    -- TODO: use bit operations
    (!h, !n2) = until ((>= 2 * n) . snd) (bimap succ (* 2)) (0 :: Int, 1 :: Int)

newLazySTreeV :: forall a op m. (Monoid a, MonoidAction op a, VU.Unbox op, PrimMonad m) => Int -> m (LazySegmentTree VM.MVector a op (PrimState m))
newLazySTreeV = newLazySTree

newLazySTreeVU :: forall a op m. (VU.Unbox a, Monoid a, MonoidAction op a, VU.Unbox op, PrimMonad m) => Int -> m (LazySegmentTree VUM.MVector a op (PrimState m))
newLazySTreeVU = newLazySTree

-- | Creates `LazySegmentTree` with initial leaf values.
generateLazySTree ::
  forall v a op m.
  (HasCallStack, VGM.MVector v a, Monoid a, MonoidAction op a, VU.Unbox op, PrimMonad m) =>
  Int ->
  (Int -> a) ->
  m (LazySegmentTree v a op (PrimState m))
generateLazySTree !n !f = do
  !as <- VGM.unsafeNew n2

  -- Create leaves:
  forMS_ (rangeMS 1 nLeaves) $ \i -> do
    if i <= n
      then VGM.write as (nLeaves + i - 1) $! f (pred i)
      else VGM.write as (nLeaves + i - 1) mempty

  -- Create parents:
  forMS_ (rangeMSR 1 (pred nLeaves)) $ \i -> do
    !l <- VGM.read as (childL i)
    !r <- VGM.read as (childR i)
    VGM.write as i (l <> r)

  !ops <- VUM.replicate n2 mempty
  return $ LazySegmentTree as ops h
  where
    -- TODO: use bit operations
    (!h, !n2) = until ((>= 2 * n) . snd) (bimap succ (* 2)) (0 :: Int, 1 :: Int)
    !nLeaves = n2 `div` 2
    childL !vertex = shiftL vertex 1
    childR !vertex = shiftL vertex 1 .|. 1

generateLazySTreeV :: forall a op m. (HasCallStack, Monoid a, MonoidAction op a, VU.Unbox op, PrimMonad m) => Int -> (Int -> a) -> m (LazySegmentTree VM.MVector a op (PrimState m))
generateLazySTreeV = generateLazySTree

generateLazySTreeVU :: forall a op m. (HasCallStack, VU.Unbox a, Monoid a, MonoidAction op a, VU.Unbox op, PrimMonad m) => Int -> (Int -> a) -> m (LazySegmentTree VUM.MVector a op (PrimState m))
generateLazySTreeVU = generateLazySTree

-- | Appends the lazy operator monoid monoids over some span of the lazy segment tree.
-- These values are just stored and performed over the nodes when queried.
updateLazySTree ::
  forall v a op m.
  (VGM.MVector v a, Monoid a, MonoidAction op a, Eq op, VU.Unbox op, PrimMonad m) =>
  LazySegmentTree v a op (PrimState m) ->
  Int ->
  Int ->
  op ->
  m ()
updateLazySTree stree@(LazySegmentTree !_ !ops !_) !iLLeaf !iRLeaf !op = do
  -- 1. Propagate the parents' lazy operator monoids into the leaves:
  _propOpMonoidsToLeaf stree iLLeaf
  _propOpMonoidsToLeaf stree iRLeaf

  -- 2. Propagate the given lazy operator monoids to the corresponding largest segments:
  let !lVertex = iLLeaf + nVerts `div` 2
      !rVertex = iRLeaf + nVerts `div` 2
  glitchLoopUpdate lVertex rVertex

  -- 3. Evaluate the parent vertices:
  _evalToRoot stree iLLeaf
  _evalToRoot stree iRLeaf

  return ()
  where
    !nVerts = VUM.length ops

    isLeftChild = not . (`testBit` 0)
    isRightChild = (`testBit` 0)

    -- Find the maximum segments for the given range and append the operator monoid.
    -- It's much like using some glitch in a platformer game:
    glitchLoopUpdate :: Int -> Int -> m ()
    glitchLoopUpdate !l !r
      | l > r = return ()
      | otherwise = do
          !l' <-
            if isRightChild l
              then do
                VUM.modify ops (<> op) l
                return $ succ l
              else return l

          !r' <-
            -- NOTE: I'm using inclusive range
            if isLeftChild r
              then do
                VUM.modify ops (<> op) r
                return $ pred r
              else return r

          -- go up to the parent segment
          glitchLoopUpdate (shiftR l' 1) (shiftR r' 1)

-- TODO: I used the top-down queries for the strict segment tree. Which is preferable?
queryLazySTree ::
  forall v a m op.
  (HasCallStack, VGM.MVector v a, Monoid a, MonoidAction op a, Eq op, VU.Unbox op, PrimMonad m) =>
  LazySegmentTree v a op (PrimState m) ->
  Int ->
  Int ->
  m a
queryLazySTree stree@(LazySegmentTree !as !ops !_) !iLLeaf !iRLeaf = do
  -- 1. Propagate the parents' lazy operator monoids into the leaves:
  _propOpMonoidsToLeaf stree iLLeaf
  _propOpMonoidsToLeaf stree iRLeaf

  -- 2. Return concatanated result:
  let !lVertex = iLLeaf + nVerts `div` 2
      !rVertex = iRLeaf + nVerts `div` 2
  glitchLoopQuery lVertex rVertex mempty mempty
  where
    !nVerts = VGM.length as

    isLeftChild = not . (`testBit` 0)
    isRightChild = (`testBit` 0)

    -- Find the maximum segments for the given range and append the value.
    -- It's much like using some glitch in a platformer game:
    glitchLoopQuery :: Int -> Int -> a -> a -> m a
    glitchLoopQuery !l !r !lAcc !rAcc
      | l > r = return $! lAcc <> rAcc
      | otherwise = do
          (!l', !lAcc') <-
            if isRightChild l
              then do
                -- Evaluate the target segmnent and append the result:
                !la' <- mact <$!> VUM.read ops l <*> VGM.read as l
                return (succ l, lAcc <> la')
              else return (l, lAcc)

          (!r', !rAcc') <-
            if isLeftChild r
              then do
                -- Evaluate the target segmnent and append the result:
                !ra' <- mact <$!> VUM.read ops r <*> VGM.read as r
                return (pred r, ra' <> rAcc)
              else return (r, rAcc)

          -- go up to the parent segment
          glitchLoopQuery (shiftR l' 1) (shiftR r' 1) lAcc' rAcc'

-- | Propagates the lazy operator monoids from top to bottom where the laef vertex is contained.
--
-- - `iLeaf`: Given with zero-based index.
_propOpMonoidsToLeaf ::
  (HasCallStack, VGM.MVector v a, MonoidAction op a, Eq op, VU.Unbox op, PrimMonad m) =>
  LazySegmentTree v a op (PrimState m) ->
  Int ->
  m ()
_propOpMonoidsToLeaf (LazySegmentTree !as !ops !height) !iLeaf = do
  let !leafVertex = iLeaf + nVerts `div` 2

  -- From parent vertex to the parent of the leaf vertex:
  forMS_ (rangeMSR 1 (pred height)) $ \iParent -> do
    let !vertex = nthParent leafVertex iParent

    -- When there's some lazy evaluation value, propagate them to their children and evaluate the vertex:
    !op <- VUM.read ops vertex
    when (op /= mempty) $ do
      -- Propagate the operator monoid to the children:
      -- REMARK: The propagated operator always comes from the right.
      VUM.modify ops (<> op) $! childL vertex
      VUM.modify ops (<> op) $! childR vertex

      -- Evaluate the vertex and consume the operator monoid:
      VGM.modify as (mact op) vertex
      VUM.write ops vertex mempty
  where
    !nVerts = VGM.length as
    nthParent !leafVertex !nth = shiftR leafVertex nth
    childL !vertex = shiftL vertex 1
    childR !vertex = shiftL vertex 1 .|. 1

-- | Evaluates parent values on `updateSegmentTree`.
-- TODO: move to where clause of the update function?
_evalToRoot ::
  (HasCallStack, VGM.MVector v a, Monoid a, MonoidAction op a, VU.Unbox op, PrimMonad m) =>
  LazySegmentTree v a op (PrimState m) ->
  Int ->
  m ()
_evalToRoot (LazySegmentTree !as !ops !height) !iLeaf = do
  let !leafVertex = iLeaf + nVerts `div` 2

  forMS_ (rangeMS 1 (pred height)) $ \iParent -> do
    let !vertex = nthParent leafVertex iParent
    let !_ = dbgAssert (vertex > 0) "_evalToRoot"

    -- Evaluate this parent node by appending the child nodes:
    !aL' <- mact <$> VUM.read ops (childL vertex) <*> VGM.read as (childL vertex)
    !aR' <- mact <$> VUM.read ops (childR vertex) <*> VGM.read as (childR vertex)
    VGM.write as vertex $! aL' <> aR'
  where
    !nVerts = VGM.length as
    nthParent !leafVertex !nth = shiftR leafVertex nth
    childL !vertex = shiftL vertex 1
    childR !vertex = shiftL vertex 1 .|. 1

-- }}}
