-- | Lazily segment tree with interval chmax/chmin.
--
-- The internals are almost entirely copy of the @Lazy@ module.
-- TODO: Do not duplicate code. Differences are LSTree nd unsafeCoerceWithLength* and _sactAt.
--
-- = Possible queries
-- Segment tree beats can also handle gcd queries, but it's not considered in this module.
module Data.SegmentTree.Beats where

import Algorithm.Bisect (bisectM)
import Control.Monad
import Control.Monad.Extra (whenM)
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

-- | Segment tree beats
data SegmentTreeBeats a op s = SegmentTreeBeats !(UM.MVector s a) !(UM.MVector s op) !Int

-- | \(O(N)\) Creates a `SegmentTreeBeats` with `mempty` as the initial accumulated values.
newSTBImpl ::
  (Monoid a, U.Unbox a, Monoid op, U.Unbox op, PrimMonad m) =>
  Int ->
  m (SegmentTreeBeats a op (PrimState m))
newSTBImpl !n = do
  !as <- GM.replicate n2 mempty
  !ops <- UM.replicate n2 mempty
  return $ SegmentTreeBeats as ops h
  where
    -- TODO: use bit operations
    (!h, !n2) = until ((>= 2 * n) . snd) (bimap succ (* 2)) (0 :: Int, 1 :: Int)

-- | \(O(N)\)
newSTB :: (U.Unbox a, Monoid a, Monoid op, U.Unbox op, PrimMonad m) => Int -> m (SegmentTreeBeats a op (PrimState m))
newSTB = newSTBImpl

-- | \(O(N)\) Creates `SegmentTreeBeats` with initial leaf values.
generateSTBImpl ::
  (HasCallStack, Monoid a, U.Unbox a, Monoid op, U.Unbox op, PrimMonad m) =>
  Int ->
  (Int -> a) ->
  m (SegmentTreeBeats a op (PrimState m))
generateSTBImpl !n !f = do
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
  return $ SegmentTreeBeats as ops h
  where
    -- TODO: use bit operations
    (!h, !n2) = until ((>= 2 * n) . snd) (bimap succ (* 2)) (0 :: Int, 1 :: Int)
    !nLeaves = n2 .>>. 1

-- | \(O(N)\)
generateSTB :: (HasCallStack, U.Unbox a, Monoid a, Monoid op, U.Unbox op, PrimMonad m) => Int -> (Int -> a) -> m (SegmentTreeBeats a op (PrimState m))
generateSTB = generateSTBImpl

-- | \(O(N)\). TODO: Share the internal implementation with `genearteSTB` takeing filling function.
{-# INLINE buildSTB #-}
buildSTB :: (HasCallStack, U.Unbox a, Monoid a, Monoid op, U.Unbox op, PrimMonad m) => U.Vector a -> m (SegmentTreeBeats a op (PrimState m))
buildSTB xs = do
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
  return $ SegmentTreeBeats as ops h
  where
    !n = U.length xs
    (!h, !n2) = until ((>= (n .<<. 1)) . snd) (bimap succ (.<<. 1)) (0 :: Int, 1 :: Int)
    !nLeaves = n2 .>>. 1

-- | \(O(\log N)\)
{-# INLINE foldSTB #-}
foldSTB ::
  forall a op m.
  (HasCallStack, FailableSemigroupActionTarget a, Monoid a, Eq a, U.Unbox a, Monoid op, SemigroupAction op a, Eq op, U.Unbox op, PrimMonad m) =>
  SegmentTreeBeats a op (PrimState m) ->
  Int ->
  Int ->
  m a
foldSTB stree = foldWithLengthSTB (unsafeCoerceWithLengthSTB stree)

-- | \(O(\log N)\)
{-# INLINE foldMaySTB #-}
foldMaySTB ::
  (HasCallStack, FailableSemigroupActionTarget a, Monoid a, Eq a, U.Unbox a, Monoid op, SemigroupAction op a, Eq op, U.Unbox op, PrimMonad m) =>
  SegmentTreeBeats a op (PrimState m) ->
  Int ->
  Int ->
  m (Maybe a)
foldMaySTB stree = foldMayWithLengthSTB (unsafeCoerceWithLengthSTB stree)

-- | \(O(\log N)\) Read one leaf. TODO: Faster implementation.
{-# INLINE readSTB #-}
readSTB ::
  (HasCallStack, FailableSemigroupActionTarget a, Monoid a, Eq a, U.Unbox a, Monoid op, SemigroupAction op a, Eq op, U.Unbox op, PrimMonad m) =>
  SegmentTreeBeats a op (PrimState m) ->
  Int ->
  m a
readSTB stree = readWithLengthSTB (unsafeCoerceWithLengthSTB stree)

-- | \(O(\log N)\)
{-# INLINE foldAllSTB #-}
foldAllSTB ::
  (HasCallStack, FailableSemigroupActionTarget a, Monoid a, Eq a, U.Unbox a, Monoid op, SemigroupAction op a, Eq op, U.Unbox op, PrimMonad m) =>
  SegmentTreeBeats a op (PrimState m) ->
  m a
foldAllSTB stree = foldAllWithLengthSTB (unsafeCoerceWithLengthSTB stree)

-- | \(O(\log N)\) Applies a lazy operator monoid over an interval, propagated lazily.
{-# INLINE sactSTB #-}
sactSTB ::
  forall a op m.
  (FailableSemigroupActionTarget a, Monoid a, Eq a, U.Unbox a, Monoid op, SemigroupAction op a, Eq op, U.Unbox op, PrimMonad m) =>
  SegmentTreeBeats a op (PrimState m) ->
  Int ->
  Int ->
  op ->
  m ()
sactSTB stree l r op = sactWithLengthSTB (unsafeCoerceWithLengthSTB stree) l r (coerce op)

-- | \(O(\log N)\) Acts on one leaf. TODO: Specialize the implementation.
{-# INLINE sactAtSTB #-}
sactAtSTB ::
  (FailableSemigroupActionTarget a, Monoid a, Eq a, U.Unbox a, Monoid op, SemigroupAction op a, Eq op, U.Unbox op, PrimMonad m) =>
  SegmentTreeBeats a op (PrimState m) ->
  Int ->
  op ->
  m ()
sactAtSTB stree i op = sactAtWithLengthSTB (unsafeCoerceWithLengthSTB stree) i (coerce op)

-- TODO: writeSTB
-- TODO: modifySTB

-- * Action with length given by the segment tree

-- | TODO: Remove @unsafe@. Maybe.
{-# INLINE unsafeCoerceWithLengthSTB #-}
unsafeCoerceWithLengthSTB :: (SemigroupAction op a) => SegmentTreeBeats a op s -> SegmentTreeBeats a (WithLength op) s
unsafeCoerceWithLengthSTB = unsafeCoerce

-- | \(O(\log N)\)
foldWithLengthSTB ::
  forall a op m.
  (HasCallStack, FailableSemigroupActionTarget a, Monoid a, Eq a, U.Unbox a, Monoid op, SemigroupActionWithLength op a, Eq op, U.Unbox op, PrimMonad m) =>
  SegmentTreeBeats a op (PrimState m) ->
  Int ->
  Int ->
  m a
foldWithLengthSTB stree@(SegmentTreeBeats !as !_ !_) !iLLeaf !iRLeaf = stToPrim $ do
  let !_ =
        dbgAssert (0 <= iLLeaf && iLLeaf <= iRLeaf && iRLeaf <= (nLeaves - 1)) $
          "foldWithLengthSTB: wrong range " ++ show (iLLeaf, iRLeaf)

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
{-# INLINE foldMayWithLengthSTB #-}
foldMayWithLengthSTB ::
  (HasCallStack, FailableSemigroupActionTarget a, Monoid a, Eq a, U.Unbox a, Monoid op, SemigroupActionWithLength op a, Eq op, U.Unbox op, PrimMonad m) =>
  SegmentTreeBeats a op (PrimState m) ->
  Int ->
  Int ->
  m (Maybe a)
foldMayWithLengthSTB stree@(SegmentTreeBeats !as !_ !_) !iLLeaf !iRLeaf
  | 0 <= iLLeaf && iLLeaf <= iRLeaf && iRLeaf <= (nLeaves - 1) =
      Just <$> foldWithLengthSTB stree iLLeaf iRLeaf
  | otherwise = return Nothing
  where
    !nLeaves = GM.length as .>>. 1

-- | \(O(\log N)\) Read one leaf. TODO: Faster implementation.
{-# INLINE readWithLengthSTB #-}
readWithLengthSTB ::
  (HasCallStack, FailableSemigroupActionTarget a, Monoid a, Eq a, U.Unbox a, Monoid op, SemigroupActionWithLength op a, Eq op, U.Unbox op, PrimMonad m) =>
  SegmentTreeBeats a op (PrimState m) ->
  Int ->
  m a
readWithLengthSTB stree i = foldWithLengthSTB stree i i

-- | \(O(\log N)\)
{-# INLINE foldAllWithLengthSTB #-}
foldAllWithLengthSTB ::
  (HasCallStack, FailableSemigroupActionTarget a, Eq a, Monoid a, U.Unbox a, Monoid op, SemigroupActionWithLength op a, Eq op, U.Unbox op, PrimMonad m) =>
  SegmentTreeBeats a op (PrimState m) ->
  m a
-- TODO: faster implementation
-- FIXME: the length must not include non-existing leaves. Remember the original length?
foldAllWithLengthSTB stree@(SegmentTreeBeats !as !_ !_) = foldWithLengthSTB stree 0 (GM.length as .>>. 1 - 1)

-- | \(O(\log N)\) Applies a lazy operator monoid over an interval, propagated lazily.
sactWithLengthSTB ::
  forall a op m.
  (FailableSemigroupActionTarget a, Monoid a, Eq a, U.Unbox a, Monoid op, SemigroupActionWithLength op a, Eq op, U.Unbox op, PrimMonad m) =>
  SegmentTreeBeats a op (PrimState m) ->
  Int ->
  Int ->
  op ->
  m ()
sactWithLengthSTB stree@(SegmentTreeBeats !as !ops !height) !iLLeaf !iRLeaf !op = stToPrim $ do
  let !_ =
        dbgAssert (0 <= iLLeaf && iLLeaf <= iRLeaf && iRLeaf <= (nLeaves - 1)) $
          "sactSTB: wrong range " ++ show (iLLeaf, iRLeaf)

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
{-# INLINE sactAtWithLengthSTB #-}
sactAtWithLengthSTB ::
  (FailableSemigroupActionTarget a, Monoid a, Eq a, U.Unbox a, Monoid op, SemigroupActionWithLength op a, Eq op, U.Unbox op, PrimMonad m) =>
  SegmentTreeBeats a op (PrimState m) ->
  Int ->
  op ->
  m ()
sactAtWithLengthSTB stree i = sactWithLengthSTB stree i i

-- TODO: writeSTB
-- TODO: modifySTB

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
  (HasCallStack, FailableSemigroupActionTarget a, Monoid a, Eq a, U.Unbox a, Monoid op, SemigroupActionWithLength op a, Eq op, U.Unbox op, PrimMonad m) =>
  SegmentTreeBeats a op (PrimState m) ->
  Int ->
  Int ->
  m ()
-- REMARK: Never INLINE this function or else it's much slower.
-- `stToPrim` also makes it slower (maybe because it's already set on the caller side?)
_propDownFromRootWithLength stree@(SegmentTreeBeats !as !_ !height) !iLeaf !lrAdjuster = do
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
  (HasCallStack, FailableSemigroupActionTarget a, Semigroup a, Eq a, U.Unbox a, Monoid op, Eq op, SemigroupActionWithLength op a, U.Unbox op, PrimMonad m) =>
  SegmentTreeBeats a op (PrimState m) ->
  Int ->
  op ->
  m ()
_sactAtWithLength stree@(SegmentTreeBeats !as !ops !height) !vertex !op = do
  -- The propagated value to the vertex is evaluated instantly:
  let !len = 1 .<<. (height - 1 - msbOf vertex)
  GM.modify as (\a -> sactWithLength op a len) vertex
  when (vertex < nLeaves) $ do
    -- The propagated value for the children are stored and propagated lazily:
    GM.modify ops (op <>) vertex
    -- Re-calculate children on bulk update failure
    whenM (isFailureFSAT <$> GM.read as vertex) $ do
      _propAtWithLength stree vertex
      l <- GM.read as (_childL vertex)
      r <- GM.read as (_childR vertex)
      GM.write as vertex $! l <> r
  where
    !nLeaves = GM.length as .>>. 1

-- | Propagates the operator onto the children. Push.
{-# INLINE _propAtWithLength #-}
_propAtWithLength ::
  (HasCallStack, FailableSemigroupActionTarget a, Semigroup a, Eq a, U.Unbox a, Monoid op, Eq op, SemigroupActionWithLength op a, U.Unbox op, PrimMonad m) =>
  SegmentTreeBeats a op (PrimState m) ->
  Int ->
  m ()
_propAtWithLength stree@(SegmentTreeBeats !_ !ops !_) !vertex = do
  -- Read and consume the operator:
  !op <- GM.exchange ops vertex mempty
  when (op /= mempty) $ do
    -- Propagate the operator onto the children:
    -- REMARK: The new coming operator operator always comes from the left.
    _sactAtWithLength stree (_childL vertex) op
    _sactAtWithLength stree (_childR vertex) op

-- * Bisection methods

-- TODO: faster implelemtaion

-- | \(O(\log^2 N)\) The @l@, @r@ indices are the zero-based leaf indices.
bisectSTB ::
  (HasCallStack, FailableSemigroupActionTarget a, Monoid a, Eq a, U.Unbox a, Monoid op, SemigroupAction op a, Eq op, U.Unbox op, PrimMonad m) =>
  SegmentTreeBeats a op (PrimState m) ->
  Int ->
  Int ->
  (a -> Bool) ->
  m (Maybe Int, Maybe Int)
bisectSTB stree@(SegmentTreeBeats !as !_ !_) l r f = do
  bisectM l r $ \r' -> do
    !acc <- foldSTB stree l r'
    return $! f acc
  where
    !_ = dbgAssert (0 <= l && l <= r && r <= nLeaves - 1) $ "bisectSTB: giveninvalid range " ++ show (l, r)
      where
        nLeaves = GM.length as .>>. 1

-- | \(O(\log^2 N)\)
bisectSTBL ::
  (HasCallStack, FailableSemigroupActionTarget a, Monoid a, Eq a, U.Unbox a, Monoid op, SemigroupAction op a, Eq op, U.Unbox op, PrimMonad m) =>
  SegmentTreeBeats a op (PrimState m) ->
  Int ->
  Int ->
  (a -> Bool) ->
  m (Maybe Int)
bisectSTBL stree l r f = fst <$> bisectSTB stree l r f

-- | \(O(\log^2 N)\)
bisectSTBR ::
  (HasCallStack, FailableSemigroupActionTarget a, Monoid a, Eq a, U.Unbox a, Monoid op, SemigroupAction op a, Eq op, U.Unbox op, PrimMonad m) =>
  SegmentTreeBeats a op (PrimState m) ->
  Int ->
  Int ->
  (a -> Bool) ->
  m (Maybe Int)
bisectSTBR stree l r f = snd <$> bisectSTB stree l r f
