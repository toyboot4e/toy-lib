{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

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
import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Data.Bifunctor
import Data.Bits
import Data.Coerce
import Data.Core.SemigroupAction
import Data.SegmentTree.Util
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Stack (HasCallStack)
import Math.BitSet (msbOf)
import ToyLib.Debug
import Unsafe.Coerce

-- | Lazy segment tree.
--
-- = (Internal) 1-based indices
--
-- Use 1-based indices for handy vertex indices:
--
-- @
-- [                  1                  ]  |
-- [        2        ] [        3        ]  | height = 4 = log_2 16
-- [   4   ] [   5   ] [   6   ] [   7   ]  |
-- [08] [09] [10] [11] [12] [13] [14] [15]  v
-- ^
-- +-- nVerts / 2
--
-- 0    1    2    3    4    5    6    7   -- iLeaf
--
-- - parent = v .>>. 1
-- - childL = v .<<. 1
-- - childR = v .<<. 1 .|. 1
-- @
--
-- = Invariant
--
-- - New operators always come from the left: @(newOp <> oldOp) acc@ or
--   @newOp `sact` (oldOp `sact` ac)@.
--
-- - Folding is performed from the left to the right. Use @Dual@ monoid if you need to reverse
--   the folding direction.
--
-- = Internals
--
-- See also:
-- <https://maspypy.com/segment-tree-%E3%81%AE%E3%81%8A%E5%8B%89%E5%BC%B72>
--
-- == Bottom-up folding
--
-- Bottom-up folding often needs less traversal than a top-down folding.
--
-- @
-- [-----------------------------]   [-]: vertices
-- [-------------] [-------------]   [*]: folded vertices ("glitch segments")
-- [-----] [*****] [*****] [-----]
-- [-] [*] [-] [-] [-] [-] [-] [-]
--     <----------------> folding interval
-- @
--
-- == Lazily propagated values for children
--
-- Each vertex has lazily propagated values. These values are stored for children and the value
-- for the vertex is applied instantly. This lets the parent evaluation be done without checking the
-- child's propagated value.
--
-- == Propagation pruning
--
-- Propagations are only performed over the parent vertices of folded vertives:
--
-- @
-- [+++++++++++++++++++++++++++++]   [-]: vertices
-- [+++++++++++++] [+++++++++++++]   [*]: folded vertices ("glitch segments")
-- [-----] [*****] [*****] [-----]   [+]: propagated vertices (above the "glitch segments")
-- [-] [*] [-] [-] [-] [-] [-] [-]
--     <----------------> folding interval
-- @
--
-- Note that the propagted vertices are pruned to be above the glitch segments only.
data LazySegmentTree a op s = LazySegmentTree !(UM.MVector s a) !(UM.MVector s op) {-# UNPACK #-} !Int

-- | \(O(N)\) Creates a `LazySegmentTree` with `mempty` as the initial accumulated values.
newLSTreeImpl ::
  (Monoid a, U.Unbox a, Monoid op, U.Unbox op, PrimMonad m) =>
  Int ->
  m (LazySegmentTree a op (PrimState m))
newLSTreeImpl !n = do
  !as <- GM.replicate n2 mempty
  !ops <- UM.replicate n2 mempty
  return $ LazySegmentTree as ops h
  where
    -- TODO: use bit operations
    (!h, !n2) = until ((>= 2 * n) . snd) (bimap succ (* 2)) (0 :: Int, 1 :: Int)

-- | \(O(N)\)
newLSTree :: (U.Unbox a, Monoid a, Monoid op, U.Unbox op, PrimMonad m) => Int -> m (LazySegmentTree a op (PrimState m))
newLSTree = newLSTreeImpl

-- | \(O(N)\) Creates `LazySegmentTree` with initial leaf values.
generateLSTreeImpl ::
  (HasCallStack, Monoid a, U.Unbox a, Monoid op, U.Unbox op, PrimMonad m) =>
  Int ->
  (Int -> a) ->
  m (LazySegmentTree a op (PrimState m))
generateLSTreeImpl !n !f = do
  !as <- GM.unsafeNew n2

  -- Fill leaves:
  forM_ [1 .. nLeaves] $ \i -> do
    if i <= n
      then GM.write as (nLeaves + i - 1) $! f (pred i)
      else GM.write as (nLeaves + i - 1) mempty

  -- Fill parents from bottom to top:
  forM_ [nLeaves - 1, nLeaves - 2 .. 1] $ \i -> do
    !l <- GM.read as (_childL i)
    !r <- GM.read as (_childR i)
    GM.write as i $! l <> r

  !ops <- UM.replicate n2 mempty
  return $ LazySegmentTree as ops h
  where
    -- TODO: use bit operations
    (!h, !n2) = until ((>= 2 * n) . snd) (bimap succ (* 2)) (0 :: Int, 1 :: Int)
    !nLeaves = n2 .>>. 1

-- | \(O(N)\)
generateLSTree :: (HasCallStack, U.Unbox a, Monoid a, Monoid op, U.Unbox op, PrimMonad m) => Int -> (Int -> a) -> m (LazySegmentTree a op (PrimState m))
generateLSTree = generateLSTreeImpl

-- | \(O(N)\). TODO: Share the internal implementation with `genearteLSTree` takeing filling function.
{-# INLINE buildLSTree #-}
buildLSTree :: (HasCallStack, U.Unbox a, Monoid a, Monoid op, U.Unbox op, PrimMonad m) => U.Vector a -> m (LazySegmentTree a op (PrimState m))
buildLSTree xs = do
  !as <- GM.unsafeNew n2

  -- Fill leaves:
  U.unsafeCopy (GM.unsafeSlice nLeaves (U.length xs) as) xs
  forM_ [U.length xs .. nLeaves - 1] $ \i ->
    GM.write as (nLeaves + i) mempty

  -- Fill parents from bottom to top:
  forM_ [nLeaves - 1, nLeaves - 2 .. 1] $ \i -> do
    !l <- GM.read as (_childL i)
    !r <- GM.read as (_childR i)
    GM.write as i $! l <> r

  !ops <- UM.replicate n2 mempty
  return $ LazySegmentTree as ops h
  where
    !n = U.length xs
    (!h, !n2) = until ((>= (n .<<. 1)) . snd) (bimap succ (.<<. 1)) (0 :: Int, 1 :: Int)
    !nLeaves = n2 .>>. 1

-- | \(O(\log N)\)
{-# INLINE foldLSTree #-}
foldLSTree ::
  forall a op m.
  (HasCallStack, Monoid a, U.Unbox a, Monoid op, SemigroupAction op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree a op (PrimState m) ->
  Int ->
  Int ->
  m a
foldLSTree stree = foldWithLengthLSTree (unsafeCoerceWithLengthLSTree stree)

-- | \(O(\log N)\)
{-# INLINE foldMayLSTree #-}
foldMayLSTree ::
  (HasCallStack, Monoid a, U.Unbox a, Monoid op, SemigroupAction op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree a op (PrimState m) ->
  Int ->
  Int ->
  m (Maybe a)
foldMayLSTree stree = foldMayWithLengthLSTree (unsafeCoerceWithLengthLSTree stree)

-- | \(O(\log N)\) Read one leaf. TODO: Faster implementation.
{-# INLINE readLSTree #-}
readLSTree ::
  (HasCallStack, Monoid a, U.Unbox a, Monoid op, SemigroupAction op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree a op (PrimState m) ->
  Int ->
  m a
readLSTree stree = readWithLengthLSTree (unsafeCoerceWithLengthLSTree stree)

-- | \(O(\log N)\)
{-# INLINE foldAllLSTree #-}
foldAllLSTree ::
  (HasCallStack, Monoid a, U.Unbox a, Monoid op, SemigroupAction op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree a op (PrimState m) ->
  m a
foldAllLSTree stree = foldAllWithLengthLSTree (unsafeCoerceWithLengthLSTree stree)

-- | \(O(\log N)\) Applies a lazy operator monoid over an interval, propagated lazily.
{-# INLINE sactLSTree #-}
sactLSTree ::
  forall a op m.
  (Semigroup a, U.Unbox a, Monoid op, SemigroupAction op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree a op (PrimState m) ->
  Int ->
  Int ->
  op ->
  m ()
sactLSTree stree l r op = sactWithLengthLSTree (unsafeCoerceWithLengthLSTree stree) l r (coerce op)

-- | \(O(\log N)\) Acts on one leaf. TODO: Specialize the implementation.
{-# INLINE sactAtLSTree #-}
sactAtLSTree ::
  (Semigroup a, U.Unbox a, Monoid op, SemigroupAction op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree a op (PrimState m) ->
  Int ->
  op ->
  m ()
sactAtLSTree stree i op = sactAtWithLengthLSTree (unsafeCoerceWithLengthLSTree stree) i (coerce op)

-- TODO: writeLSTree
-- TODO: modifyLSTree

-- * Action with length given by the segment tree

-- | \(O(\log N)\)
foldWithLengthLSTree ::
  forall a op m.
  (HasCallStack, Monoid a, U.Unbox a, Monoid op, SemigroupActionWithLength op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree a op (PrimState m) ->
  Int ->
  Int ->
  m a
foldWithLengthLSTree stree@(LazySegmentTree !as !_ !_) !iLLeaf !iRLeaf = stToPrim $ do
  let !_ =
        dbgAssert (0 <= iLLeaf && iLLeaf <= iRLeaf && iRLeaf <= (nLeaves - 1)) $
          "foldWithLengthLSTree: wrong range " ++ show (iLLeaf, iRLeaf)

  -- 1. Propagate the parents' lazy operator monoids and evaluate up to the acted vertices:
  _propDownFromRootWithLength stree iLLeaf 0
  _propDownFromRootWithLength stree iRLeaf 1

  -- 2. Fold:
  glitchFold (iLLeaf + nLeaves) (iRLeaf + nLeaves) mempty mempty
  where
    !nLeaves = GM.length as .>>. 1

    -- \(O(\log N)\)
    -- glitchFold :: Int -> Int -> a -> a -> m a
    glitchFold !l !r !lAcc !rAcc
      | l > r = return $! lAcc <> rAcc
      | otherwise = do
          -- Note that the operator at @i@ is already performed for @i@ (it' for their children).
          !lAcc' <-
            if _isRChild l
              then (lAcc <>) <$> GM.read as l
              else return lAcc

          !rAcc' <-
            if _isLChild r
              then (<> rAcc) <$> GM.read as r
              else return rAcc

          -- go up to the parent segment, but optionally out of the bounds (like a glitch):
          glitchFold ((l + 1) .>>. 1) ((r - 1) .>>. 1) lAcc' rAcc'

-- | \(O(\log N)\)
{-# INLINE foldMayWithLengthLSTree #-}
foldMayWithLengthLSTree ::
  (HasCallStack, Monoid a, U.Unbox a, Monoid op, SemigroupActionWithLength op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree a op (PrimState m) ->
  Int ->
  Int ->
  m (Maybe a)
foldMayWithLengthLSTree stree@(LazySegmentTree !as !_ !_) !iLLeaf !iRLeaf
  | 0 <= iLLeaf && iLLeaf <= iRLeaf && iRLeaf <= (nLeaves - 1) =
      Just <$> foldWithLengthLSTree stree iLLeaf iRLeaf
  | otherwise = return Nothing
  where
    !nLeaves = GM.length as .>>. 1

-- | \(O(\log N)\) Read one leaf. TODO: Faster implementation.
{-# INLINE readWithLengthLSTree #-}
readWithLengthLSTree ::
  (HasCallStack, Monoid a, U.Unbox a, Monoid op, SemigroupActionWithLength op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree a op (PrimState m) ->
  Int ->
  m a
readWithLengthLSTree stree i = foldWithLengthLSTree stree i i

-- | \(O(\log N)\)
{-# INLINE foldAllWithLengthLSTree #-}
foldAllWithLengthLSTree ::
  (HasCallStack, Monoid a, U.Unbox a, Monoid op, SemigroupActionWithLength op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree a op (PrimState m) ->
  m a
-- TODO: faster implementation
-- FIXME: the length must not include non-existing leaves. Remember the original length?
foldAllWithLengthLSTree stree@(LazySegmentTree !as !_ !_) = foldWithLengthLSTree stree 0 (GM.length as .>>. 1 - 1)

-- | \(O(\log N)\) Applies a lazy operator monoid over an interval, propagated lazily.
sactWithLengthLSTree ::
  forall a op m.
  (Semigroup a, U.Unbox a, Monoid op, SemigroupActionWithLength op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree a op (PrimState m) ->
  Int ->
  Int ->
  op ->
  m ()
sactWithLengthLSTree stree@(LazySegmentTree !as !ops !height) !iLLeaf !iRLeaf !op = stToPrim $ do
  let !_ =
        dbgAssert (0 <= iLLeaf && iLLeaf <= iRLeaf && iRLeaf <= (nLeaves - 1)) $
          "sactLSTree: wrong range " ++ show (iLLeaf, iRLeaf)

  -- 1. Propagate the parents' lazy operator monoids and evaluate up to the acted vertices:
  _propDownFromRootWithLength stree iLLeaf 0
  _propDownFromRootWithLength stree iRLeaf 1

  -- 2. Propagate the given lazy operator monoids to the corresponding segments:
  glitchSAct (iLLeaf + nLeaves) (iRLeaf + nLeaves)

  -- 3. Evaluate the parent vertices:
  evalParents (iLLeaf + nLeaves) 0
  evalParents (iRLeaf + nLeaves) 1
  where
    !nLeaves = GM.length ops .>>. 1

    -- \(O(\log N)\)
    -- glitchSAct :: Int -> Int -> m ()
    glitchSAct !l !r
      | l > r = return ()
      | otherwise = do
          when (_isRChild l) $ _sactAtWithLength stree l op
          when (_isLChild r) $ _sactAtWithLength stree r op
          -- go up to the parent segment, but optionally out of the bounds (like a glitch):
          glitchSAct ((l + 1) .>>. 1) ((r - 1) .>>. 1)

    -- \(O(N)\) Evaluates parent values of glitch intervals.
    -- evalParents :: Int -> Int -> m ()
    evalParents !leafVertex !lrAdjuster = do
      forM_ [1 .. pred height] $ \iParent -> do
        let !v = leafVertex .>>. iParent
        when (_pruneTrick leafVertex iParent lrAdjuster) $ do
          l <- GM.read as (_childL v)
          r <- GM.read as (_childR v)
          GM.write as v $! l <> r

-- | \(O(\log N)\) Acts on one leaf. TODO: Specialize the implementation.
{-# INLINE sactAtWithLengthLSTree #-}
sactAtWithLengthLSTree ::
  (Semigroup a, U.Unbox a, Monoid op, SemigroupActionWithLength op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree a op (PrimState m) ->
  Int ->
  op ->
  m ()
sactAtWithLengthLSTree stree i = sactWithLengthLSTree stree i i

-- TODO: writeLSTree
-- TODO: modifyLSTree

-- | \(O(\log N)\) Propagates the lazy operator monoids from the root to just before the glitch
-- segments.
--
-- - `iLeaf`: Given with zero-based index.
--
-- = Pruning
-- The propagation is performed from the root to just before the folded vertices. In other words,
-- propagation is performed just before performing the first glitch. That's enough for both folding
-- and acting.
_propDownFromRootWithLength ::
  (HasCallStack, U.Unbox a, Monoid op, SemigroupActionWithLength op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree a op (PrimState m) ->
  Int ->
  Int ->
  m ()
-- REMARK: Never INLINE this function or else it's much slower.
-- `stToPrim` also makes it slower (maybe because it's already set on the caller side?)
_propDownFromRootWithLength stree@(LazySegmentTree !as !_ !height) !iLeaf !lrAdjuster = do
  let !leafVertex = iLeaf + nLeaves
  -- From parent vertex to the parent of the leaf vertex:
  forM_ [height - 1, height - 2 .. 1] $ \iParent -> do
    when (_pruneTrick leafVertex iParent lrAdjuster) $ do
      _propAtWithLength stree $ leafVertex .>>. iParent
  where
    !nLeaves = GM.length as .>>. 1

-- | \(O(1)\) Acts on a node.
--
-- = Evaluation strategy
-- - The propagated value for the children are stored and propagated lazily.
-- - The propagated value to the vertex is evaluated instantly.
--
-- = Invariants
-- - The new coming operator operator always comes from the left.
{-# INLINE _sactAtWithLength #-}
_sactAtWithLength ::
  (HasCallStack, U.Unbox a, Semigroup op, SemigroupActionWithLength op a, U.Unbox op, PrimMonad m) =>
  LazySegmentTree a op (PrimState m) ->
  Int ->
  op ->
  m ()
_sactAtWithLength (LazySegmentTree !as !ops !height) !vertex !op = do
  -- The propagated value to the vertex is evaluated instantly:
  let !len = 1 .<<. (height - 1 - msbOf vertex)
  GM.modify as (\a -> sactWithLength op a len) vertex
  when (vertex < nLeaves) $ do
    -- The propagated value for the children are stored and propagated lazily:
    GM.modify ops (op <>) vertex
  where
    !nLeaves = GM.length as .>>. 1

-- | Propagates the operator onto the children. Push.
{-# INLINE _propAtWithLength #-}
_propAtWithLength ::
  (HasCallStack, U.Unbox a, Monoid op, Eq op, SemigroupActionWithLength op a, U.Unbox op, PrimMonad m) =>
  LazySegmentTree a op (PrimState m) ->
  Int ->
  m ()
_propAtWithLength stree@(LazySegmentTree !_ !ops !_) !vertex = do
  -- Read and consume the operator:
  !op <- GM.exchange ops vertex mempty
  when (op /= mempty) $ do
    -- Propagate the operator onto the children:
    -- REMARK: The new coming operator operator always comes from the left.
    _sactAtWithLength stree (_childL vertex) op
    _sactAtWithLength stree (_childR vertex) op

-- * Bisection methods

-- | TODO: Remove @unsafe@. Maybe.
{-# INLINE unsafeCoerceWithLengthLSTree #-}
unsafeCoerceWithLengthLSTree :: (SemigroupAction op a) => LazySegmentTree a op s -> LazySegmentTree a (WithLength op) s
unsafeCoerceWithLengthLSTree = unsafeCoerce

-- TODO: faster implelemtaion

-- | \(O(\log^2 N)\) The @l@, @r@ indices are the zero-based leaf indices.
bisectLSTree ::
  (HasCallStack, Monoid a, U.Unbox a, Monoid op, SemigroupAction op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree a op (PrimState m) ->
  Int ->
  Int ->
  (a -> Bool) ->
  m (Maybe Int, Maybe Int)
bisectLSTree stree@(LazySegmentTree !as !_ !_) l r f = do
  bisectM l r $ \r' -> do
    !acc <- foldLSTree stree l r'
    return $! f acc
  where
    !_ = dbgAssert (0 <= l && l <= r && r <= nLeaves - 1) $ "bisectLSTree: giveninvalid range " ++ show (l, r)
      where
        nLeaves = GM.length as .>>. 1

-- | \(O(\log^2 N)\)
bisectLSTreeL ::
  (HasCallStack, Monoid a, U.Unbox a, Monoid op, SemigroupAction op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree a op (PrimState m) ->
  Int ->
  Int ->
  (a -> Bool) ->
  m (Maybe Int)
bisectLSTreeL stree l r f = fst <$> bisectLSTree stree l r f

-- | \(O(\log^2 N)\)
bisectLSTreeR ::
  (HasCallStack, Monoid a, U.Unbox a, Monoid op, SemigroupAction op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree a op (PrimState m) ->
  Int ->
  Int ->
  (a -> Bool) ->
  m (Maybe Int)
bisectLSTreeR stree l r f = snd <$> bisectLSTree stree l r f
