-- | Lazily propageted segment tree, where we can perform operation over range.
--
-- = Algebra
-- - (Op * Acc) <> (Op * Acc) = (Op <> Op) * (Acc <> Acc)
--
-- Typical example of @Op@ is a matrix and @Acc@ is a column vector.
--
-- = Typical problems
-- - [Typical 029 - Long Bricks (★5)](https://atcoder.jp/contests/typical90/tasks/typical90_ac)
-- - [EDPC W - Intervals](https://atcoder.jp/contests/dp/tasks/dp_w)
module Data.SegmentTree.Lazy where

import Algorithm.Bisect (bisectM)
import Control.Monad
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bifunctor
import Data.Bits
import Data.Core.SemigroupAction
import Data.Ix (inRange)
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Stack (HasCallStack)
import ToyLib.Debug

-- | Lazy segment tree.
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
-- = Invariant
--
-- - New operators always come from the left: @(newOp <> oldOp) acc@ or
-- @newOp `sact` (oldOp `sact` ac)@.
data LazySegmentTree v a op s = LazySegmentTree !(v s a) !(UM.MVector s op) !Int

-- | \(O(N)\) Creates `LazySegmentTree` with `mempty` as the initial accumulated values.
newLSTreeImpl ::
  forall a op v m.
  (Monoid a, Monoid op, U.Unbox op, GM.MVector v a, PrimMonad m) =>
  Int ->
  m (LazySegmentTree v a op (PrimState m))
newLSTreeImpl !n = do
  !as <- GM.replicate n2 mempty
  !ops <- UM.replicate n2 mempty
  return $ LazySegmentTree as ops h
  where
    -- TODO: use bit operations
    (!h, !n2) = until ((>= 2 * n) . snd) (bimap succ (* 2)) (0 :: Int, 1 :: Int)

-- | \(O(N)\)
newLSTree :: forall a op m. (U.Unbox a, Monoid a, Monoid op, U.Unbox op, PrimMonad m) => Int -> m (LazySegmentTree UM.MVector a op (PrimState m))
newLSTree = newLSTreeImpl

-- | \(O(N)\) Creates `LazySegmentTree` with initial leaf values.
generateLSTreeImpl ::
  forall v a op m.
  (HasCallStack, GM.MVector v a, Monoid a, Monoid op, U.Unbox op, PrimMonad m) =>
  Int ->
  (Int -> a) ->
  m (LazySegmentTree v a op (PrimState m))
generateLSTreeImpl !n !f = do
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
    childL !vertex = vertex .<<. 1
    childR !vertex = vertex .<<. 1 .|. 1

-- | \(O(N)\)
generateLSTree :: forall a op m. (HasCallStack, U.Unbox a, Monoid a, Monoid op, U.Unbox op, PrimMonad m) => Int -> (Int -> a) -> m (LazySegmentTree UM.MVector a op (PrimState m))
generateLSTree = generateLSTreeImpl

-- | \(O(N)\). TODO: Test it. Share the internal implementation with `genearteLSTree` takeing filling function.
{-# INLINE buildLSTree #-}
buildLSTree :: forall op a m. (HasCallStack, U.Unbox a, Monoid a, Monoid op, U.Unbox op, PrimMonad m) => U.Vector a -> m (LazySegmentTree UM.MVector a op (PrimState m))
buildLSTree xs = do
  !as <- GM.unsafeNew n2

  -- Fill leaves:
  U.unsafeCopy (GM.unsafeSlice nLeaves (U.length xs) as) xs
  forM_ [U.length xs .. nLeaves - 1] $ \i ->
    GM.write as (nLeaves + i) mempty

  -- Fill parents from bottom to top:
  forM_ [nLeaves - 1, nLeaves - 2 .. 1] $ \i -> do
    !l <- GM.read as (childL i)
    !r <- GM.read as (childR i)
    GM.write as i $! l <> r

  !ops <- UM.replicate n2 mempty
  return $ LazySegmentTree as ops h
  where
    !n = U.length xs
    (!h, !n2) = until ((>= (n .<<. 1)) . snd) (bimap succ (.<<. 1)) (0 :: Int, 1 :: Int)
    !nLeaves = n2 `div` 2
    childL !vertex = vertex .<<. 1
    childR !vertex = vertex .<<. 1 .|. 1

-- | \(O(\log N)\) Appends the lazy operator monoid monoids over a span. They are just stored and
-- propagated when queried.
sactLSTree ::
  forall v a op m.
  (GM.MVector v a, Monoid a, Monoid op, SemigroupAction op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree v a op (PrimState m) ->
  Int ->
  Int ->
  op ->
  m ()
sactLSTree stree@(LazySegmentTree !_ !ops !_) !iLLeaf !iRLeaf !op = do
  let !_ =
        dbgAssert (inRange (0, nLeaves - 1) iLLeaf && inRange (0, nLeaves - 1) iRLeaf) $
          "sactLSTree: wrong range " ++ show (iLLeaf, iRLeaf)

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
          when (isRightChild l) $ do
            -- REMARK: The new coming operator operator always comes from the left.
            UM.modify ops (op <>) l

          when (isLeftChild r) $ do
            -- REMARK: The new coming operator operator always comes from the left.
            UM.modify ops (op <>) r

          -- go up to the parent segment
          glitchLoopUpdate ((l + 1) .>>. 1) ((r - 1) .>>. 1)

-- | \(O(\log N)\) Acts on one leaf. TODO: Faster implementation.
sactAtLSTree ::
  forall v a op m.
  (GM.MVector v a, Monoid a, Monoid op, SemigroupAction op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree v a op (PrimState m) ->
  Int ->
  op ->
  m ()
sactAtLSTree stree i = sactLSTree stree i i

-- | \(O(\log N)\)
foldLSTree ::
  forall v a m op.
  (HasCallStack, GM.MVector v a, Monoid a, Monoid op, SemigroupAction op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree v a op (PrimState m) ->
  Int ->
  Int ->
  m a
foldLSTree stree@(LazySegmentTree !as !ops !_) !iLLeaf !iRLeaf = do
  let !_ =
        dbgAssert (iLLeaf <= iRLeaf && inRange (0, nLeaves - 1) iLLeaf && inRange (0, nLeaves - 1) iRLeaf) $
          "foldLSTree: wrong range " ++ show (iLLeaf, iRLeaf)

  -- 1. Propagate the parents' lazy operator monoids into the leaves:
  _propOpMonoidsToLeaf stree iLLeaf
  _propOpMonoidsToLeaf stree iRLeaf

  -- 2. Return concatanated result:
  let !lVertex = iLLeaf + nLeaves
      !rVertex = iRLeaf + nLeaves
  glitchFold lVertex rVertex mempty mempty
  where
    !nLeaves = GM.length as `div` 2

    isLeftChild = not . (`testBit` 0)
    isRightChild = (`testBit` 0)

    -- Find the maximum segments for the given range and append the value.
    -- It's much like using some glitch in a platformer game:
    glitchFold :: Int -> Int -> a -> a -> m a
    glitchFold !l !r !lAcc !rAcc
      | l > r = return $! lAcc <> rAcc
      | otherwise = do
          !lAcc' <-
            if isRightChild l
              then do
                -- Evaluate the target segmnent and append the result:
                !la' <- (lAcc <>) <$> (sact <$> UM.read ops l <*> GM.read as l)
                return la'
              else return lAcc

          !rAcc' <-
            if isLeftChild r
              then do
                -- Evaluate the target segmnent and append the result:
                !ra' <- (<> rAcc) <$> (sact <$> UM.read ops r <*> GM.read as r)
                return ra'
              else return rAcc

          -- go up to the parent segment
          glitchFold ((l + 1) .>>. 1) ((r - 1) .>>. 1) lAcc' rAcc'

-- | \(O(\log N)\) Read one leaf. TODO: Faster implementation.
readLSTree ::
  forall a op v m.
  (HasCallStack, GM.MVector v a, Monoid a, Monoid op, SemigroupAction op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree v a op (PrimState m) ->
  Int ->
  m a
readLSTree stree i = foldLSTree stree i i

-- | \(O(\log N)\)
foldMayLSTree ::
  forall a op v m.
  (HasCallStack, GM.MVector v a, Monoid a, Monoid op, SemigroupAction op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree v a op (PrimState m) ->
  Int ->
  Int ->
  m (Maybe a)
-- TODO: faster impl
foldMayLSTree stree@(LazySegmentTree !as !_ !_) !iLLeaf !iRLeaf
  | iLLeaf > iRLeaf || not (inRange (0, nLeaves - 1) iLLeaf) || not (inRange (0, nLeaves - 1) iRLeaf) = return Nothing
  | otherwise = Just <$> foldLSTree stree iLLeaf iRLeaf
  where
    !nLeaves = GM.length as `div` 2

-- | \(O(\log N)\)
foldAllLSTree ::
  forall a op v m.
  (HasCallStack, GM.MVector v a, Monoid a, Monoid op, SemigroupAction op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree v a op (PrimState m) ->
  m a
-- TODO: faster implementation
foldAllLSTree stree@(LazySegmentTree !as !_ !_) = foldLSTree stree 0 (GM.length as - 1)

-- | \(O(\log N)\) Propagates the lazy operator monoids from top to bottom where the leaf vertex is contained.
--
-- - `iLeaf`: Given with zero-based index.
_propOpMonoidsToLeaf ::
  (HasCallStack, GM.MVector v a, Monoid op, SemigroupAction op a, Eq op, U.Unbox op, PrimMonad m) =>
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
      GM.modify as (sact op) vertex
      UM.write ops vertex mempty
  where
    !nVerts = GM.length as
    nthParent !leafVertex !nth = leafVertex .>>. nth
    childL !vertex = vertex .<<. 1
    childR !vertex = vertex .<<. 1 .|. 1

-- | Evaluates parent values on `updateSegmentTree`.
-- TODO: move to where clause of the update function?
_evalToRoot ::
  (HasCallStack, GM.MVector v a, Monoid a, SemigroupAction op a, U.Unbox op, PrimMonad m) =>
  LazySegmentTree v a op (PrimState m) ->
  Int ->
  m ()
_evalToRoot (LazySegmentTree !as !ops !height) !iLeaf = do
  let !leafVertex = iLeaf + nVerts `div` 2

  forM_ [1 .. pred height] $ \iParent -> do
    let !vertex = nthParent leafVertex iParent
    let !_ = dbgAssert (vertex > 0) "_evalToRoot"

    -- Evaluate this parent node by appending the child nodes:
    !aL' <- sact <$!> UM.read ops (childL vertex) <*> GM.read as (childL vertex)
    !aR' <- sact <$!> UM.read ops (childR vertex) <*> GM.read as (childR vertex)
    GM.write as vertex $! aL' <> aR'
  where
    !nVerts = GM.length as
    nthParent !leafVertex !nth = leafVertex .>>. nth
    childL !vertex = vertex .<<. 1
    childR !vertex = vertex .<<. 1 .|. 1

----------------------------------------------------------------------------------------------------
-- TODO: test them
----------------------------------------------------------------------------------------------------

-- | \(O(\log^2 N)\) The @l@, @r@ indices are the zero-based leaf indices.
{-# INLINE bisectLSTree #-}
bisectLSTree ::
  forall v a m op.
  (HasCallStack, GM.MVector v a, Monoid a, Monoid op, SemigroupAction op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree v a op (PrimState m) ->
  Int ->
  Int ->
  (a -> Bool) ->
  m (Maybe Int, Maybe Int)
bisectLSTree stree@(LazySegmentTree !as !_ !_) l r f = do
  bisectM l r $ \r' -> do
    !acc <- foldLSTree stree l r'
    return $! f acc
  where
    !_ = dbgAssert (inRange (0, nLeaves - 1) l && inRange (0, nLeaves - 1) r) $ "bisectLSTree: giveninvalid range " ++ show (l, r)
      where
        nLeaves = GM.length as `div` 2

-- | \(O(\log^2 N)\)
{-# INLINE bisectLSTreeL #-}
bisectLSTreeL ::
  forall v a m op.
  (HasCallStack, GM.MVector v a, Monoid a, Monoid op, SemigroupAction op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree v a op (PrimState m) ->
  Int ->
  Int ->
  (a -> Bool) ->
  m (Maybe Int)
bisectLSTreeL stree l r f = fst <$> bisectLSTree stree l r f

-- | \(O(\log^2 N)\)
{-# INLINE bisectLSTreeR #-}
bisectLSTreeR ::
  forall v a m op.
  (HasCallStack, GM.MVector v a, Monoid a, Monoid op, SemigroupAction op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree v a op (PrimState m) ->
  Int ->
  Int ->
  (a -> Bool) ->
  m (Maybe Int)
bisectLSTreeR stree l r f = snd <$> bisectLSTree stree l r f
