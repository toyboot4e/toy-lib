{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
-- TODO: write manually
import Data.Core.SemigroupAction
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Stack (HasCallStack)
import Math.BitSet (msbOf)
import ToyLib.Debug
import Data.Coerce

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
data LazySegmentTree a op s = LazySegmentTree !(UM.MVector s a) !(UM.MVector s op) !Int

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

-- | \(O(1)\)
{-# INLINE _childL #-}
_childL :: Int -> Int
_childL !vertex = vertex .<<. 1

-- | \(O(1)\)
{-# INLINE _childR #-}
_childR :: Int -> Int
_childR !vertex = (vertex .<<. 1) .|. 1

-- | \(O(1)\)
{-# INLINE _isLChild #-}
_isLChild :: Int -> Bool
_isLChild = not . (`testBit` 0)

-- | \(O(1)\)
{-# INLINE _isRChild #-}
_isRChild :: Int -> Bool
_isRChild = (`testBit` 0)

-- | \(O(1)\) Pruning trick for excluding vertices under the glitch segments.
{-# INLINE _pruneTrick #-}
_pruneTrick :: Int -> Int -> Int -> Bool
_pruneTrick vLeaf iParent lrAdjuster = (vLeaf + lrAdjuster) .>>. iParent .<<. iParent /= (vLeaf + lrAdjuster)

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

-- * Semigroup action without length

-- | \(O(\log N)\)
{-# INLINE foldLSTree #-}
foldLSTree ::
  forall a op m.
  (HasCallStack, Monoid a, U.Unbox a, Monoid op, SemigroupAction op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree a op (PrimState m) ->
  Int ->
  Int ->
  m a
foldLSTree = _foldLSTree sactWithoutLength

-- | \(O(\log N)\)
{-# INLINE foldMayLSTree #-}
foldMayLSTree ::
  (HasCallStack, Monoid a, U.Unbox a, Monoid op, SemigroupAction op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree a op (PrimState m) ->
  Int ->
  Int ->
  m (Maybe a)
foldMayLSTree = _foldMayLSTree sactWithoutLength

-- | \(O(\log N)\) Reads one leaf.
{-# INLINE readLSTree #-}
readLSTree ::
  (HasCallStack, Monoid a, U.Unbox a, Monoid op, SemigroupAction op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree a op (PrimState m) ->
  Int ->
  m a
readLSTree = _readLSTree sactWithoutLength

-- | \(O(\log N)\)
{-# INLINE foldAllLSTree #-}
foldAllLSTree ::
  (HasCallStack, Monoid a, U.Unbox a, Monoid op, SemigroupAction op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree a op (PrimState m) ->
  m a
foldAllLSTree = _foldAllLSTree sactWithoutLength

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
sactLSTree = _sactLSTree sactWithoutLength

-- | \(O(\log N)\) Acts on one leaf. TODO: Specialize the implementation.
{-# INLINE sactAtLSTree #-}
sactAtLSTree ::
  (Semigroup a, U.Unbox a, Monoid op, SemigroupAction op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree a op (PrimState m) ->
  Int ->
  op ->
  m ()
sactAtLSTree = _sactAtLSTree sactWithoutLength

-- TODO: writeLSTree
-- TODO: modifyLSTree

-- * Semigroup action with length

-- | Right semigroup aciton with length given by the segment tree. Any `Semigroup` can be coerced
-- into `SemigroupActionWithLength` using the `WithLength` newtype.
class SemigroupActionWithLength s a where
  -- | Right semigroup aciton with length given by the segment tree.
  sactWithLength :: s -> a -> Int -> a

{-# INLINE sactWithoutLength #-}
sactWithoutLength :: (SemigroupAction s a) => s -> a -> Int -> a
sactWithoutLength s a _ = sact s a

-- | (Internal) Wraps a `SemigroupAction` instance into a `SemigroupActionWithLength`.
newtype WithLength a = WithLength a
  deriving newtype (Eq, Ord)

instance (Semigroup a) => Semigroup (WithLength a) where
  {-# INLINE (<>) #-}
  (<>) = coerce ((<>) @a)

instance (Monoid a) => Monoid (WithLength a) where
  {-# INLINE mempty #-}
  mempty = coerce (mempty @a)

instance (SemigroupAction s a) => SemigroupActionWithLength (WithLength s) a where
  {-# INLINE sactWithLength #-}
  sactWithLength s a _ = coerce (sact @s @a) s a

-- | \(O(\log N)\)
{-# INLINE foldWithLengthLSTree #-}
foldWithLengthLSTree ::
  forall a op m.
  (HasCallStack, Monoid a, U.Unbox a, Monoid op, SemigroupActionWithLength op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree a op (PrimState m) ->
  Int ->
  Int ->
  m a
foldWithLengthLSTree = _foldLSTree sactWithLength

-- | \(O(\log N)\)
{-# INLINE foldMayWithLengthLSTree #-}
foldMayWithLengthLSTree ::
  (HasCallStack, Monoid a, U.Unbox a, Monoid op, SemigroupActionWithLength op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree a op (PrimState m) ->
  Int ->
  Int ->
  m (Maybe a)
foldMayWithLengthLSTree = _foldMayLSTree sactWithLength

-- | \(O(\log N)\) Read one leaf. TODO: Faster implementation.
{-# INLINE readWithLengthLSTree #-}
readWithLengthLSTree ::
  (HasCallStack, Monoid a, U.Unbox a, Monoid op, SemigroupActionWithLength op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree a op (PrimState m) ->
  Int ->
  m a
readWithLengthLSTree = _readLSTree sactWithLength

-- | \(O(\log N)\)
{-# INLINE foldAllWithLengthLSTree #-}
foldAllWithLengthLSTree ::
  (HasCallStack, Monoid a, U.Unbox a, Monoid op, SemigroupActionWithLength op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree a op (PrimState m) ->
  m a
foldAllWithLengthLSTree = _foldAllLSTree sactWithLength

-- | \(O(\log N)\) Applies a lazy operator monoid over an interval, propagated lazily.
{-# INLINE sactWithLengthLSTree #-}
sactWithLengthLSTree ::
  forall a op m.
  (Semigroup a, U.Unbox a, Monoid op, SemigroupActionWithLength op a, Eq op, U.Unbox op, PrimMonad m) =>
  LazySegmentTree a op (PrimState m) ->
  Int ->
  Int ->
  op ->
  m ()
sactWithLengthLSTree = _sactLSTree sactWithLength

-- | \(O(\log N)\) Acts on one leaf. TODO: Specialize the implementation.
{-# INLINE sactAtWithLengthLSTree #-}
sactAtWithLengthLSTree ::
  (U.Unbox a, Monoid op, SemigroupActionWithLength op a, U.Unbox op, PrimMonad m) =>
  LazySegmentTree a op (PrimState m) ->
  Int ->
  op ->
  m ()
sactAtWithLengthLSTree = _sactAt sactWithLength

-- TODO: writeLSTree
-- TODO: modifyLSTree

-- * Bisection methods

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

-- * Internals

-- | \(O(\log N)\)
_foldLSTree ::
  forall a op m.
  (HasCallStack, Monoid a, U.Unbox a, Monoid op, Eq op, U.Unbox op, PrimMonad m) =>
  (op -> a -> Int -> a) ->
  LazySegmentTree a op (PrimState m) ->
  Int ->
  Int ->
  m a
_foldLSTree sact_ stree@(LazySegmentTree !as !_ !_) !iLLeaf !iRLeaf = do
  let !_ =
        dbgAssert (0 <= iLLeaf && iLLeaf <= iRLeaf && iRLeaf <= (nLeaves - 1)) $
          "_foldLSTree: invalid range " ++ show (iLLeaf, iRLeaf)

  -- 1. Propagate the parents' lazy operator monoids and evaluate up to the acted vertices:
  _propDownFromRoot sact_ stree iLLeaf 0
  _propDownFromRoot sact_ stree iRLeaf 1

  -- 2. Fold:
  glitchFold (iLLeaf + nLeaves) (iRLeaf + nLeaves) mempty mempty
  where
    !nLeaves = GM.length as .>>. 1

    -- \(O(\log N)\)
    glitchFold :: Int -> Int -> a -> a -> m a
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
_foldMayLSTree ::
  (HasCallStack, Monoid a, U.Unbox a, Monoid op, Eq op, U.Unbox op, PrimMonad m) =>
  (op -> a -> Int -> a) ->
  LazySegmentTree a op (PrimState m) ->
  Int ->
  Int ->
  m (Maybe a)
_foldMayLSTree sact_ stree@(LazySegmentTree !as !_ !_) !iLLeaf !iRLeaf
  | 0 <= iLLeaf && iLLeaf <= iRLeaf && iRLeaf <= (nLeaves - 1) =
    Just <$> _foldLSTree sact_ stree iLLeaf iRLeaf
  | otherwise = return Nothing
  where
    !nLeaves = GM.length as .>>. 1

-- | \(O(\log N)\) Read one leaf. TODO: Faster implementation.
_readLSTree ::
  (HasCallStack, Monoid a, U.Unbox a, Monoid op, Eq op, U.Unbox op, PrimMonad m) =>
  (op -> a -> Int -> a) ->
  LazySegmentTree a op (PrimState m) ->
  Int ->
  m a
_readLSTree _sact stree i = _foldLSTree _sact stree i i

-- | \(O(\log N)\)
_foldAllLSTree ::
  (HasCallStack, Monoid a, U.Unbox a, Monoid op, Eq op, U.Unbox op, PrimMonad m) =>
  (op -> a -> Int -> a) ->
  LazySegmentTree a op (PrimState m) ->
  m a
-- TODO: faster implementation
_foldAllLSTree sact_ stree@(LazySegmentTree !as !_ !_) = _foldLSTree sact_ stree 0 (GM.length as .>>. 1 - 1)

-- | \(O(\log N)\) Applies a lazy operator monoid over an interval, propagated lazily.
_sactLSTree ::
  forall a op m.
  (Semigroup a, U.Unbox a, Monoid op, Eq op, U.Unbox op, PrimMonad m) =>
  (op -> a -> Int -> a) ->
  LazySegmentTree a op (PrimState m) ->
  Int ->
  Int ->
  op ->
  m ()
_sactLSTree sact_ stree@(LazySegmentTree !as !ops !height) !iLLeaf !iRLeaf !op = do
  let !_ =
        dbgAssert (0 <= iLLeaf && iLLeaf <= iRLeaf && iRLeaf <= (nLeaves - 1)) $
          "sactLSTree: wrong range " ++ show (iLLeaf, iRLeaf)

  -- 1. Propagate the parents' lazy operator monoids and evaluate up to the acted vertices:
  _propDownFromRoot sact_ stree iLLeaf 0
  _propDownFromRoot sact_ stree iRLeaf 1

  -- 2. Propagate the given lazy operator monoids to the corresponding segments:
  glitchSAct (iLLeaf + nLeaves) (iRLeaf + nLeaves)

  -- 3. Evaluate the parent vertices:
  evalParents (iLLeaf + nLeaves) 0
  evalParents (iRLeaf + nLeaves) 1
  where
    !nLeaves = GM.length ops .>>. 1

    -- \(O(\log N)\)
    glitchSAct :: Int -> Int -> m ()
    glitchSAct !l !r
      | l > r = return ()
      | otherwise = do
          when (_isRChild l) $ _sactAt sact_ stree l op
          when (_isLChild r) $ _sactAt sact_ stree r op
          -- go up to the parent segment, but optionally out of the bounds (like a glitch):
          glitchSAct ((l + 1) .>>. 1) ((r - 1) .>>. 1)

    -- \(O(N)\) Evaluates parent values of glitch intervals.
    evalParents :: Int -> Int -> m ()
    evalParents !leafVertex !lrAdjuster = do
      forM_ [1 .. pred height] $ \iParent -> do
        let !v = leafVertex .>>. iParent
        when (_pruneTrick leafVertex iParent lrAdjuster) $ do
          l <- GM.read as (_childL v)
          r <- GM.read as (_childR v)
          GM.write as v $! l <> r

-- | \(O(\log N)\) Acts on one leaf. TODO: Specialize the implementation.
_sactAtLSTree ::
  (Semigroup a, U.Unbox a, Monoid op, Eq op, U.Unbox op, PrimMonad m) =>
  (op -> a -> Int -> a) ->
  LazySegmentTree a op (PrimState m) ->
  Int ->
  op ->
  m ()
_sactAtLSTree sact_ stree i op = _sactLSTree sact_ stree i i op

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
_propDownFromRoot ::
  (HasCallStack, U.Unbox a, Monoid op, Eq op, U.Unbox op, PrimMonad m) =>
  (op -> a -> Int -> a) ->
  LazySegmentTree a op (PrimState m) ->
  Int ->
  Int ->
  m ()
_propDownFromRoot _sact stree@(LazySegmentTree !as !_ !height) !iLeaf !lrAdjuster = do
  let !leafVertex = iLeaf + nLeaves
  -- From parent vertex to the parent of the leaf vertex:
  forM_ [height - 1, height - 2 .. 1] $ \iParent -> do
    when (_pruneTrick leafVertex iParent lrAdjuster) $ do
      _propAt _sact stree $ leafVertex .>>. iParent
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
_sactAt ::
  (HasCallStack, U.Unbox a, Semigroup op, U.Unbox op, PrimMonad m) =>
  (op -> a -> Int -> a) ->
  LazySegmentTree a op (PrimState m) ->
  Int ->
  op ->
  m ()
_sactAt sact_ (LazySegmentTree !as !ops !height) !vertex !op = do
  -- The propagated value to the vertex is evaluated instantly:
  let !len = 1 .<<. (height - 1 - msbOf vertex)
  GM.modify as (\a -> sact_ op a len) vertex
  when (vertex < nLeaves) $ do
    -- The propagated value for the children are stored and propagated lazily:
    -- TODO: beats
    GM.modify ops (op <>) vertex
  where
    !nLeaves = GM.length as .>>. 1

-- | Propagates the operator onto the children. Push.
_propAt ::
  (HasCallStack, U.Unbox a, Monoid op, Eq op, U.Unbox op, PrimMonad m) =>
  (op -> a -> Int -> a) ->
  LazySegmentTree a op (PrimState m) ->
  Int ->
  m ()
_propAt sact_ stree@(LazySegmentTree !_ !ops !_) !vertex = do
  -- Read and consume the operator:
  !op <- GM.exchange ops vertex mempty
  when (op /= mempty) $ do
    -- Propagate the operator onto the children:
    -- REMARK: The new coming operator operator always comes from the left.
    _sactAt sact_ stree (_childL vertex) op
    _sactAt sact_ stree (_childR vertex) op

