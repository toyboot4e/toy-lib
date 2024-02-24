-- | Lazily propageted segment tree, where we can perform operation over range.
--
-- = Algebra
-- - (Op * Acc) <> (Op * Acc) = (Op <> Op) (Acc <> Acc)
--
-- = Typical problems
-- - [Typical 029 - Long Bricks (★5)](https://atcoder.jp/contests/typical90/tasks/typical90_ac)
-- - [EDPC W - Intervals](https://atcoder.jp/contests/dp/tasks/dp_w)
--
-- - TODO: Add `set` / `get`, as in [ac-library](https://github.com/atcoder/ac-library/blob/master/atcoder/lazysegtree.hpp)
-- - TODO: consider Node/Act naming rather than Acc/Op.
module Data.SegmentTree.Lazy where

import Control.Monad
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bifunctor
import Data.Bits
import Data.Core.SemigroupAction
import Data.Ix (inRange)
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Stack (HasCallStack)
import ToyLib.Macro

-- {{{ Lazy segment tree

-- TODO: Do we have to duplicate `SegmentTree` and `LazySegmentTree`?
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
-- - New operators always come from the left: @(newOp <> oldOp) acc@ or @newOp `sact` (oldOp `sact ac)@.
data LazySegmentTree v a op s = LazySegmentTree !(v s a) !(UM.MVector s op) !Int

-- | Creates `LazySegmentTree` with `mempty` as the initial accumulated values.
newLazySTree ::
  forall v a op m.
  (GM.MVector v a, Monoid a, MonoidAction op a, U.Unbox op, PrimMonad m) =>
  Int ->
  m (LazySegmentTree v a op (PrimState m))
newLazySTree !n = do
  !as <- GM.replicate n2 mempty
  !ops <- UM.replicate n2 mempty
  return $ LazySegmentTree as ops h
  where
    -- TODO: use bit operations
    (!h, !n2) = until ((>= 2 * n) . snd) (bimap succ (* 2)) (0 :: Int, 1 :: Int)

newLazySTreeV :: forall a op m. (Monoid a, MonoidAction op a, U.Unbox op, PrimMonad m) => Int -> m (LazySegmentTree VM.MVector a op (PrimState m))
newLazySTreeV = newLazySTree

newLazySTreeU :: forall a op m. (U.Unbox a, Monoid a, MonoidAction op a, U.Unbox op, PrimMonad m) => Int -> m (LazySegmentTree UM.MVector a op (PrimState m))
newLazySTreeU = newLazySTree

-- | Creates `LazySegmentTree` with initial leaf values.
generateLazySTreeG ::
  forall v a op m.
  (HasCallStack, GM.MVector v a, Monoid a, MonoidAction op a, U.Unbox op, PrimMonad m) =>
  Int ->
  (Int -> a) ->
  m (LazySegmentTree v a op (PrimState m))
generateLazySTreeG !n !f = do
  !as <- GM.unsafeNew n2

  -- Fill leaves:
  forM_ [1 .. nLeaves] $ \i -> do
    if i <= n
      then GM.write as (nLeaves + i - 1) $! f (pred i)
      else GM.write as (nLeaves + i - 1) mempty

  -- Fill parents from bottom to top:
  forM_ [nLeaves - 1, nLeaves - 2 .. 1] $ \i -> do
    !l <- GM.read as (childL i)
    !r <- GM.read as (childR i)
    GM.write as i $! l <> r

  !ops <- UM.replicate n2 mempty
  return $ LazySegmentTree as ops h
  where
    -- TODO: use bit operations
    (!h, !n2) = until ((>= 2 * n) . snd) (bimap succ (* 2)) (0 :: Int, 1 :: Int)
    !nLeaves = n2 `div` 2
    childL !vertex = shiftL vertex 1
    childR !vertex = shiftL vertex 1 .|. 1

generateLazySTreeV :: forall a op m. (HasCallStack, Monoid a, MonoidAction op a, U.Unbox op, PrimMonad m) => Int -> (Int -> a) -> m (LazySegmentTree VM.MVector a op (PrimState m))
generateLazySTreeV = generateLazySTreeG

generateLazySTreeU :: forall a op m. (HasCallStack, U.Unbox a, Monoid a, MonoidAction op a, U.Unbox op, PrimMonad m) => Int -> (Int -> a) -> m (LazySegmentTree UM.MVector a op (PrimState m))
generateLazySTreeU = generateLazySTreeG

-- | Appends the lazy operator monoid monoids over a span. They are just stored and propagated when
-- queried.
updateLazySTree ::
  forall v a op m.
  (GM.MVector v a, Monoid a, MonoidAction op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree v a op (PrimState m) ->
  Int ->
  Int ->
  op ->
  m ()
updateLazySTree stree@(LazySegmentTree !_ !ops !_) !iLLeaf !iRLeaf !op = do
  let !_ =
        dbgAssert (inRange (0, nLeaves - 1) iLLeaf && inRange (0, nLeaves - 1) iRLeaf) $
          "updateLazySTree: wrong range " ++ show (iLLeaf, iRLeaf)

  -- 1. Propagate the parents' lazy operator monoids into the leaves:
  _propOpMonoidsToLeaf stree iLLeaf
  _propOpMonoidsToLeaf stree iRLeaf

  -- 2. Propagate the given lazy operator monoids to the corresponding largest segments:
  let !lVertex = iLLeaf + nLeaves
      !rVertex = iRLeaf + nLeaves
  glitchLoopUpdate lVertex rVertex

  -- 3. Evaluate the parent vertices:
  _evalToRoot stree iLLeaf
  _evalToRoot stree iRLeaf

  return ()
  where
    !nLeaves = UM.length ops `div` 2

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
                -- REMARK: The new coming operator operator always comes from the left.
                UM.modify ops (op <>) l
                return $ succ l
              else return l

          !r' <-
            -- NOTE: I'm using inclusive range
            if isLeftChild r
              then do
                -- REMARK: The new coming operator operator always comes from the left.
                UM.modify ops (op <>) r
                return $ pred r
              else return r

          -- go up to the parent segment
          glitchLoopUpdate (shiftR l' 1) (shiftR r' 1)

queryLazySTree ::
  forall v a m op.
  (HasCallStack, GM.MVector v a, Monoid a, MonoidAction op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree v a op (PrimState m) ->
  Int ->
  Int ->
  m a
queryLazySTree stree@(LazySegmentTree !as !ops !_) !iLLeaf !iRLeaf = do
  let !_ =
        dbgAssert (inRange (0, nLeaves - 1) iLLeaf && inRange (0, nLeaves - 1) iRLeaf) $
          "queryLazySTree: wrong range " ++ show (iLLeaf, iRLeaf)

  -- 1. Propagate the parents' lazy operator monoids into the leaves:
  _propOpMonoidsToLeaf stree iLLeaf
  _propOpMonoidsToLeaf stree iRLeaf

  -- 2. Return concatanated result:
  let !lVertex = iLLeaf + nLeaves
      !rVertex = iRLeaf + nLeaves
  glitchLoopQuery lVertex rVertex mempty mempty
  where
    !nLeaves = GM.length as `div` 2

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
                !la' <- mact <$!> UM.read ops l <*> GM.read as l
                return (succ l, lAcc <> la')
              else return (l, lAcc)

          (!r', !rAcc') <-
            if isLeftChild r
              then do
                -- Evaluate the target segmnent and append the result:
                !ra' <- mact <$!> UM.read ops r <*> GM.read as r
                let !ra'' = ra' <> rAcc
                return (pred r, ra'')
              else return (r, rAcc)

          -- go up to the parent segment
          glitchLoopQuery (shiftR l' 1) (shiftR r' 1) lAcc' rAcc'

-- | Propagates the lazy operator monoids from top to bottom where the leaf vertex is contained.
--
-- - `iLeaf`: Given with zero-based index.
_propOpMonoidsToLeaf ::
  (HasCallStack, GM.MVector v a, MonoidAction op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree v a op (PrimState m) ->
  Int ->
  m ()
_propOpMonoidsToLeaf (LazySegmentTree !as !ops !height) !iLeaf = do
  let !leafVertex = iLeaf + nVerts `div` 2

  -- From parent vertex to the parent of the leaf vertex:
  forM_ [height - 1, height - 2 .. 1] $ \iParent -> do
    let !vertex = nthParent leafVertex iParent

    -- When there's some lazy evaluation value, propagate them to their children and evaluate the vertex:
    !op <- UM.read ops vertex
    when (op /= mempty) $ do
      -- Propagate the operator monoid to the children:
      -- REMARK: The new coming operator operator always comes from the left.
      UM.modify ops (op <>) $! childL vertex
      UM.modify ops (op <>) $! childR vertex

      -- Evaluate the vertex and consume the operator monoid:
      GM.modify as (mact op) vertex
      UM.write ops vertex mempty
  where
    !nVerts = GM.length as
    nthParent !leafVertex !nth = shiftR leafVertex nth
    childL !vertex = shiftL vertex 1
    childR !vertex = shiftL vertex 1 .|. 1

-- | Evaluates parent values on `updateSegmentTree`.
-- TODO: move to where clause of the update function?
_evalToRoot ::
  (HasCallStack, GM.MVector v a, Monoid a, MonoidAction op a, U.Unbox op, PrimMonad m) =>
  LazySegmentTree v a op (PrimState m) ->
  Int ->
  m ()
_evalToRoot (LazySegmentTree !as !ops !height) !iLeaf = do
  let !leafVertex = iLeaf + nVerts `div` 2

  forM_ [1 .. pred height] $ \iParent -> do
    let !vertex = nthParent leafVertex iParent
    let !_ = dbgAssert (vertex > 0) "_evalToRoot"

    -- Evaluate this parent node by appending the child nodes:
    !aL' <- mact <$!> UM.read ops (childL vertex) <*> GM.read as (childL vertex)
    !aR' <- mact <$!> UM.read ops (childR vertex) <*> GM.read as (childR vertex)
    GM.write as vertex $! aL' <> aR'
  where
    !nVerts = GM.length as
    nthParent !leafVertex !nth = shiftR leafVertex nth
    childL !vertex = shiftL vertex 1
    childR !vertex = shiftL vertex 1 .|. 1

-- }}}
