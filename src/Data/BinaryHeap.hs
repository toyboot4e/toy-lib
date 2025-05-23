{-# LANGUAGE RecordWildCards #-}

-- | The ultra fast binary heap taken from [cojna/iota](https://github.com/cojna/iota). Thanks!
--
-- = API
--
-- == Construction
--
-- - `newMinBH`, `buildMinBH`
-- - `newMaxBH`, `buildMaxBH`
--
-- == Operations and views
--
-- - `insertBH`
-- - `deleteBH`
-- - `peekBH`
-- - `deleteBH`
-- - `unsafeDeleteBH`
--
-- == Reset
--
-- - `clearBH`
module Data.BinaryHeap where

import Control.Monad
import Control.Monad.Primitive
import Data.Bits
import Data.Coerce
import Data.Function
import Data.Functor.Identity
import Data.Kind
import Data.Ord
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Stack (HasCallStack)

-- TODO: separate key/value?

-- | Binary heap backed by a vector.
--
-- = 0-based index
--
-- Left child is given as @i .<<. 1 + 1@. Right child is left child + 1. Parent is @(i - 1) .>>. 1@.
--
-- @
--     0
--   1   2
--  3 4 5 6
-- @
--
-- = Invariant
--
-- Parent value is smaller than or equal to their children.
data BinaryHeap (f :: Type -> Type) s a = BinaryHeap
  { -- | Newtype of `OrdVia`
    priorityBH :: !(a -> f a),
    -- | Stores the size of the heap.
    sizeBH_ :: !(UM.MVector s Int),
    -- | The storage.
    internalVecBH :: !(UM.MVector s a)
  }

-- TODO: consider using type parameter for the newtype?

type MinBinaryHeap s a = BinaryHeap Identity s a

type MaxBinaryHeap s a = BinaryHeap Down s a

-- | \(O(N)\)
_newBH :: (U.Unbox a, PrimMonad m) => (a -> f a) -> Int -> m (BinaryHeap f (PrimState m) a)
_newBH prio n = BinaryHeap prio <$> UM.replicate 1 0 <*> UM.unsafeNew n

-- | \(O(N)\)
newMinBH :: (U.Unbox a, PrimMonad m) => Int -> m (MinBinaryHeap (PrimState m) a)
newMinBH = _newBH Identity

-- | \(O(N)\)
newMaxBH :: (U.Unbox a, PrimMonad m) => Int -> m (MaxBinaryHeap (PrimState m) a)
newMaxBH = _newBH Down

-- TODO: BH
sizeBH :: (PrimMonad m) => BinaryHeap f (PrimState m) a -> m Int
sizeBH BinaryHeap {..} = UM.unsafeRead sizeBH_ 0
{-# INLINE sizeBH #-}

-- | \(O(\log N)\) Moves a leaf value upwards order to keep the invariant.
siftUpBy ::
  (U.Unbox a, PrimMonad m) =>
  (a -> a -> Ordering) ->
  Int ->
  UM.MVector (PrimState m) a ->
  m ()
siftUpBy cmp k vec = do
  x <- UM.unsafeRead vec k
  flip fix k $ \loop !i ->
    if i > 0
      then do
        let parent = (i - 1) `unsafeShiftR` 1
        p <- UM.unsafeRead vec parent
        case cmp p x of
          -- p > child: swap
          GT -> UM.unsafeWrite vec i p >> loop parent
          -- child <= p: done
          _ -> UM.unsafeWrite vec i x
      else UM.unsafeWrite vec 0 x
{-# INLINE siftUpBy #-}

-- | \(O(\log N)\) Moves a parent vertex downwards in order to keep the invariant.
siftDownBy ::
  (U.Unbox a, PrimMonad m) =>
  (a -> a -> Ordering) ->
  Int ->
  UM.MVector (PrimState m) a ->
  m ()
siftDownBy cmp k vec = do
  x <- UM.unsafeRead vec k
  let !n = UM.length vec
  flip fix k $ \loop !i -> do
    let l = unsafeShiftL i 1 .|. 1
    let r = l + 1
    if n <= l
      then do
        -- no children: done.
        UM.unsafeWrite vec i x
      else do
        vl <- UM.unsafeRead vec l
        if r < n
          then do
            -- two children.
            vr <- UM.unsafeRead vec r
            case cmp vr vl of
              -- vr < vl
              LT -> case cmp x vr of
                -- vr < vl, vr < x: go down to the right. chances are, the right subtree is lower.
                GT -> UM.unsafeWrite vec i vr >> loop r
                -- x <= vr < vl. done
                _ -> UM.unsafeWrite vec i x
              -- vl <= vr
              _ -> case cmp x vl of
                -- vl <= vr, vl < x: go down to the left.
                GT -> UM.unsafeWrite vec i vl >> loop l
                _ -> UM.unsafeWrite vec i x
          else case cmp x vl of
            -- left child only.
            GT -> UM.unsafeWrite vec i vl >> loop l
            _ -> UM.unsafeWrite vec i x
{-# INLINE siftDownBy #-}

-- | \(O(N \log N)\) Sorts a vector as a heap.
heapifyBy ::
  (U.Unbox a, PrimMonad m) =>
  (a -> a -> Ordering) ->
  UM.MVector (PrimState m) a ->
  m ()
heapifyBy cmp vec = do
  let n = UM.length vec `quot` 2
  forM_ [n - 1, n - 2 .. 0] $ \i -> do
    siftDownBy cmp i vec
{-# INLINE heapifyBy #-}

-- | Compare via a newtype.
class OrdVia f a where
  compareVia :: (a -> f a) -> a -> a -> Ordering

instance (Ord a) => OrdVia Identity a where
  compareVia _ = coerce (compare :: Identity a -> Identity a -> Ordering)
  {-# INLINE compareVia #-}

instance (Ord a) => OrdVia Down a where
  compareVia _ = coerce (compare :: Down a -> Down a -> Ordering)
  {-# INLINE compareVia #-}

-- | \(O(N \log N)\) Creates a binary heap with a newtype comparator and a vector.
buildBHVia ::
  (OrdVia f a, U.Unbox a, PrimMonad m) =>
  (a -> f a) ->
  U.Vector a ->
  m (BinaryHeap f (PrimState m) a)
buildBHVia priorityBH vec = do
  sizeBH_ <- UM.replicate 1 $ U.length vec
  internalVecBH <- U.thaw vec
  heapifyBy (compareVia priorityBH) internalVecBH
  pure $! BinaryHeap {..}
{-# INLINE buildBHVia #-}

-- | \(O(N \log N)\) Creates a `BinaryHeap` from a vector.
buildMinBinaryHeap ::
  (Ord a, U.Unbox a, PrimMonad m) =>
  U.Vector a ->
  m (BinaryHeap Identity (PrimState m) a)
buildMinBinaryHeap = buildBHVia Identity
{-# INLINE buildMinBinaryHeap #-}

-- | \(O(N \log N)\) Creates a `BinaryHeap` from a vector.
buildMaxBinaryHeap ::
  (Ord a, U.Unbox a, PrimMonad m) =>
  U.Vector a ->
  m (BinaryHeap Down (PrimState m) a)
buildMaxBinaryHeap = buildBHVia Down
{-# INLINE buildMaxBinaryHeap #-}

-- | \(O(1)\) Reads the top node. Returns an unknown value when the heap is emtpy.
unsafeViewBH ::
  (U.Unbox a, PrimMonad m) =>
  BinaryHeap f (PrimState m) a ->
  m a
unsafeViewBH BinaryHeap {..} = UM.unsafeRead internalVecBH 0
{-# INLINE unsafeViewBH #-}

-- | \(O(1)\) Reads the top nodes.
peekBH ::
  (U.Unbox a, PrimMonad m) =>
  BinaryHeap f (PrimState m) a ->
  m (Maybe a)
peekBH bh = do
  size <- sizeBH bh
  if size > 0
    then Just <$!> unsafeViewBH bh
    else pure Nothing
{-# INLINE peekBH #-}

-- | \(O(\log N)\) Inserts a value as a leaf and sorts upwards.
insertBH ::
  (OrdVia f a, U.Unbox a, PrimMonad m) =>
  BinaryHeap f (PrimState m) a ->
  a ->
  m ()
insertBH BinaryHeap {..} x = do
  size <- UM.unsafeRead sizeBH_ 0
  UM.unsafeWrite sizeBH_ 0 (size + 1)
  UM.unsafeWrite internalVecBH size x
  siftUpBy (compareVia priorityBH) size internalVecBH
{-# INLINE insertBH #-}

-- | \(O(\log N)\) Deletes the top node. Returns nothing.
unsafeDeleteBH_ ::
  (OrdVia f a, U.Unbox a, PrimMonad m) =>
  BinaryHeap f (PrimState m) a ->
  m ()
unsafeDeleteBH_ BinaryHeap {..} = do
  size' <- subtract 1 <$!> UM.unsafeRead sizeBH_ 0
  UM.unsafeWrite sizeBH_ 0 size'
  UM.unsafeSwap internalVecBH 0 size'
  siftDownBy (compareVia priorityBH) 0 (UM.unsafeTake size' internalVecBH)
{-# INLINE unsafeDeleteBH_ #-}

-- | \(O(\log N)\) Deletes and returns the top node.
unsafeDeleteBH ::
  (OrdVia f a, U.Unbox a, PrimMonad m) =>
  BinaryHeap f (PrimState m) a ->
  m a
unsafeDeleteBH bh = unsafeViewBH bh <* unsafeDeleteBH_ bh
{-# INLINE unsafeDeleteBH #-}

-- | \(O(\log N)\) Modifies the top node of the heap.
modifyBH ::
  (OrdVia f a, U.Unbox a, PrimMonad m) =>
  BinaryHeap f (PrimState m) a ->
  (a -> a) ->
  m ()
modifyBH BinaryHeap {..} f = do
  UM.unsafeModify internalVecBH f 0
  size <- UM.unsafeRead sizeBH_ 0
  siftDownBy (compareVia priorityBH) 0 (UM.unsafeTake size internalVecBH)
{-# INLINE modifyBH #-}

-- | \(O(\log N)\) Deletes and returns the top node.
{-# INLINE deleteBH #-}
deleteBH ::
  (HasCallStack, OrdVia f a, U.Unbox a, PrimMonad m) =>
  BinaryHeap f (PrimState m) a ->
  m a
deleteBH bh = do
  size <- sizeBH bh
  if size > 0
    then unsafeDeleteBH bh
    else error "tried to remove the top value from an empty binary heap"

-- | \(O(\log N)\) Optionally deletes and returns the top node.
{-# INLINE deleteMaybeBH #-}
deleteMaybeBH ::
  (OrdVia f a, U.Unbox a, PrimMonad m) =>
  BinaryHeap f (PrimState m) a ->
  m (Maybe a)
deleteMaybeBH bh = do
  size <- sizeBH bh
  if size > 0
    then Just <$> unsafeDeleteBH bh
    else pure Nothing

-- | \(O(1)\) Clears the heap.
clearBH :: (PrimMonad m) => BinaryHeap f (PrimState m) a -> m ()
clearBH BinaryHeap {..} = UM.unsafeWrite sizeBH_ 0 0

-- | \(O(1)\) TODO: It is not sorted, right?
unsafeFreezeBH ::
  (U.Unbox a, PrimMonad m) =>
  BinaryHeap f (PrimState m) a ->
  m (U.Vector a)
unsafeFreezeBH BinaryHeap {..} = do
  size <- UM.unsafeRead sizeBH_ 0
  U.unsafeFreeze (UM.unsafeTake size internalVecBH)
