{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | A mutable splay tree-based sequence.
--
-- = Typical Problems
module Data.SplaySeq where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Core.SegmentAction
import Data.Pool (PoolIndex(..))
import Data.SplaySeq.Raw
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Stack (HasCallStack)

-- | Mutable, splay tree-based sequences.
--
-- Internally, it's `RawSplaySeq` with automatic root index update.
data SplaySeq s v a = SplaySeq
  { -- | The maximum number of elements.
    rawSS :: !(RawSplaySeq s v a),
    -- | The root vertex.
    rootSS :: !(UM.MVector s SplayIndex)
  }

-- | \(O(N)\) Allocates a `SplaySeq` with lazily propagated values.
{-# INLINE newLazySS #-}
newLazySS :: (U.Unbox v, U.Unbox a, PrimMonad m) => Int -> m (SplaySeq (PrimState m) v a)
newLazySS n = do
  rawSS <- newRSS n
  rootSS <- UM.replicate 1 $ PoolIndex (-1)
  return SplaySeq {..}

-- | \(O(N)\) Allocates a `SplaySeq` without lazily propagated values. It still allocates needless
-- vector, but I failed to fix it.
{-# INLINE newSS #-}
newSS :: (U.Unbox v, PrimMonad m) => Int -> m (SplaySeq (PrimState m) v v)
newSS = newLazySS

-- | \(O(1)\) Returns the maximum number of elements.
{-# INLINE capacitySS #-}
capacitySS :: SplaySeq s v a -> Int
capacitySS = capacityRSS . rawSS

-- | \(O(1)\) Returns the number of elements stored. Requires monad for tracking the state.
{-# INLINE sizeSS #-}
sizeSS :: (PrimMonad m) => SplaySeq (PrimState m) v a -> m Int
sizeSS = sizeRSS . rawSS

-- | \(O(1)\) Returns the number of elements in a sequence.
{-# INLINE seqSizeSS #-}
seqSizeSS :: (HasCallStack, PrimMonad m) => SplaySeq (PrimState m) v a -> m Int
seqSizeSS SplaySeq {..} = do
  root <- GM.unsafeRead rootSS 0
  seqSizeRSS rawSS root

-- * Allocation

-- | \(O(N)\) Allocates a new sequence, internally as a binary tree from the bottom to the top.
allocSeqSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a) => SplaySeq (PrimState m) v a -> U.Vector v -> m ()
allocSeqSS SplaySeq {..} xs = do
  root <- allocSeqRSS rawSS xs
  GM.unsafeWrite rootSS 0 root

-- | \(O(N)\) Frees itself.
freeSS :: (HasCallStack, PrimMonad m) => SplaySeq (PrimState m) v a -> m ()
freeSS SplaySeq {..} =
  GM.unsafeModifyM
    rootSS
    ( \root -> do
        freeSubtreeRSS rawSS root
        return undefSI
    )
    0

-- * API

-- | Amortized \(O(\log N)\). Reads a kth node's value.
{-# INLINE readSS #-}
readSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SegmentAction a v, Eq a) => SplaySeq (PrimState m) v a -> Int -> m v
readSS SplaySeq {..} k = do
  root <- GM.unsafeRead rootSS 0
  (!x, !root') <- readRSS rawSS k root
  GM.unsafeWrite rootSS 0 root'
  return x

-- | Amortized \(O(\log N)\). Writes a kth node's value.
{-# INLINE writeSS #-}
writeSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SegmentAction a v, Eq a) => SplaySeq (PrimState m) v a -> Int -> v -> m ()
writeSS SplaySeq {..} k v = do
  GM.unsafeModifyM rootSS (writeRSS rawSS k v) 0

-- | Amortized \(O(\log N)\). Modifies a kth node's value.
{-# INLINE modifySS #-}
modifySS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SegmentAction a v, Eq a) => SplaySeq (PrimState m) v a -> (v -> v) -> Int -> m ()
modifySS SplaySeq {..} f k = do
  GM.unsafeModifyM rootSS (modifyRSS rawSS f k) 0

-- | Amortized \(O(\log N)\). Exchanges a kth node's value.
{-# INLINE exchangeSS #-}
exchangeSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SegmentAction a v, Eq a) => SplaySeq (PrimState m) v a -> Int -> v -> m v
exchangeSS SplaySeq {..} k v = do
  root <- GM.unsafeRead rootSS 0
  (!x, !root') <- exchangeRSS rawSS k v root
  GM.unsafeWrite rootSS 0 root'
  return x

-- | Amortized \(O(\log N)\). Folds an interval @[l, r]@.
{-# INLINE foldSS #-}
foldSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SegmentAction a v, Eq a) => SplaySeq (PrimState m) v a -> Int -> Int -> m v
foldSS SplaySeq {..} l r = do
  root <- GM.unsafeRead rootSS 0
  (!x, !root') <- foldRSS rawSS l r root
  GM.unsafeWrite rootSS 0 root'
  return x

-- | Amortized \(O(\log N)\). Folds the whole sequence.
{-# INLINE foldAllSS #-}
foldAllSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SegmentAction a v, Eq a) => SplaySeq (PrimState m) v a -> m v
foldAllSS SplaySeq {..} = do
  root <- GM.unsafeRead rootSS 0
  foldAllRSS rawSS root

-- | Amortized \(O(\log N)\).
{-# INLINE sactSS #-}
sactSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SegmentAction a v, Eq a) => SplaySeq (PrimState m) v a -> Int -> Int -> a -> m ()
sactSS SplaySeq {..} l r act = do
  GM.unsafeModifyM rootSS (sactRSS rawSS l r act) 0

-- | Amortised \(O(\log N)\). Reverses the order of nodes in given range @[l, r]@. Requires the
-- monoid and the action to be commutative.
{-# INLINE reverseSS #-}
reverseSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SegmentAction a v, Eq a) => SplaySeq (PrimState m) v a -> Int -> Int -> m ()
reverseSS SplaySeq {..} l r = do
  GM.unsafeModifyM rootSS (reverseRSS rawSS l r) 0

-- | Amortized \(O(\log N)\). Inserts a node at @k@.
{-# INLINE insertSS #-}
insertSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SegmentAction a v, Eq a) => SplaySeq (PrimState m) v a -> Int -> v -> m ()
insertSS SplaySeq {..} k v = do
  GM.unsafeModifyM rootSS (insertRSS rawSS k v) 0

-- | Amortized \(O(\log N)\). Deletes a node at @k@.
{-# INLINE deleteSS #-}
deleteSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SegmentAction a v, Eq a) => SplaySeq (PrimState m) v a -> Int -> m ()
deleteSS SplaySeq {..} i = do
  GM.unsafeModifyM rootSS (deleteRSS rawSS i) 0

-- | Amortized \(O(\log N)\). Bisection method over the sequence. Partition point. Note that The
-- user function is run over each node, not fold of an interval.
{-# INLINE bisectLSS #-}
bisectLSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SegmentAction a v, Eq a) => SplaySeq (PrimState m) v a -> (v -> Bool) -> m (Maybe SplayIndex)
bisectLSS SplaySeq {..} check = do
  root <- GM.unsafeRead rootSS 0
  (!res, !root') <- bisectLRSS rawSS check root
  GM.unsafeWrite rootSS 0 root'
  return res
