{-# LANGUAGE RecordWildCards #-}

-- | The ultra fast binary heap taken from [cojna/iota](https://github.com/cojna/iota). Thanks!
--
-- = API
--
-- == Construction
--
-- - `newMinBinaryHeap`, `buildMinBinaryHeapVia`
-- - `newMaxBinaryHeap`, `buildMaxBinaryHeapVia`
--
-- == Operations and views
--
-- - `insertBH`,
-- - `deleteFindBH`
-- - `viewBH`
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

data BinaryHeap (f :: Type -> Type) s a = BinaryHeap
  { priorityBH :: !(a -> f a),
    intVarsBH :: !(UM.MVector s Int),
    internalVecBH :: !(UM.MVector s a)
  }

_sizeBH :: Int
_sizeBH = 0
{-# INLINE _sizeBH #-}

type MinBinaryHeap s a = BinaryHeap Identity s a

type MaxBinaryHeap s a = BinaryHeap Down s a

newBinaryHeap :: (U.Unbox a, PrimMonad m) => (a -> f a) -> Int -> m (BinaryHeap f (PrimState m) a)
newBinaryHeap prio n = BinaryHeap prio <$> UM.replicate 1 0 <*> UM.unsafeNew n

newMinBinaryHeap :: (U.Unbox a, PrimMonad m) => Int -> m (MinBinaryHeap (PrimState m) a)
newMinBinaryHeap = newBinaryHeap Identity

newMaxBinaryHeap :: (U.Unbox a, PrimMonad m) => Int -> m (MaxBinaryHeap (PrimState m) a)
newMaxBinaryHeap = newBinaryHeap Down

getBinaryHeapSize :: (PrimMonad m) => BinaryHeap f (PrimState m) a -> m Int
getBinaryHeapSize BinaryHeap {..} = UM.unsafeRead intVarsBH _sizeBH
{-# INLINE getBinaryHeapSize #-}

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
          GT -> UM.unsafeWrite vec i p >> loop parent
          _ -> UM.unsafeWrite vec i x
      else UM.unsafeWrite vec 0 x
{-# INLINE siftUpBy #-}

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
      then UM.unsafeWrite vec i x
      else do
        vl <- UM.unsafeRead vec l
        if r < n
          then do
            vr <- UM.unsafeRead vec r
            case cmp vr vl of
              LT -> case cmp x vr of
                GT -> UM.unsafeWrite vec i vr >> loop r
                _ -> UM.unsafeWrite vec i x
              _ -> case cmp x vl of
                GT -> UM.unsafeWrite vec i vl >> loop l
                _ -> UM.unsafeWrite vec i x
          else case cmp x vl of
            GT -> UM.unsafeWrite vec i vl >> loop l
            _ -> UM.unsafeWrite vec i x
{-# INLINE siftDownBy #-}

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

class OrdVia f a where
  compareVia :: (a -> f a) -> a -> a -> Ordering

instance (Ord a) => OrdVia Identity a where
  compareVia _ = coerce (compare :: Identity a -> Identity a -> Ordering)
  {-# INLINE compareVia #-}

instance (Ord a) => OrdVia Down a where
  compareVia _ = coerce (compare :: Down a -> Down a -> Ordering)
  {-# INLINE compareVia #-}

buildBinaryHeapVia ::
  (OrdVia f a, U.Unbox a, PrimMonad m) =>
  (a -> f a) ->
  U.Vector a ->
  m (BinaryHeap f (PrimState m) a)
buildBinaryHeapVia priorityBH vec = do
  intVarsBH <- UM.replicate 1 $ U.length vec
  internalVecBH <- U.thaw vec
  heapifyBy (compareVia priorityBH) internalVecBH
  return $! BinaryHeap {..}
{-# INLINE buildBinaryHeapVia #-}

buildMinBinaryHeap ::
  (Ord a, U.Unbox a, PrimMonad m) =>
  U.Vector a ->
  m (BinaryHeap Identity (PrimState m) a)
buildMinBinaryHeap = buildBinaryHeapVia Identity
{-# INLINE buildMinBinaryHeap #-}

buildMaxBinaryHeap ::
  (Ord a, U.Unbox a, PrimMonad m) =>
  U.Vector a ->
  m (BinaryHeap Down (PrimState m) a)
buildMaxBinaryHeap = buildBinaryHeapVia Down
{-# INLINE buildMaxBinaryHeap #-}

unsafeViewBH ::
  (U.Unbox a, PrimMonad m) =>
  BinaryHeap f (PrimState m) a ->
  m a
unsafeViewBH BinaryHeap {..} = UM.unsafeRead internalVecBH 0
{-# INLINE unsafeViewBH #-}

viewBH ::
  (U.Unbox a, PrimMonad m) =>
  BinaryHeap f (PrimState m) a ->
  m (Maybe a)
viewBH bh = do
  size <- getBinaryHeapSize bh
  if size > 0
    then Just <$!> unsafeViewBH bh
    else return Nothing
{-# INLINE viewBH #-}

insertBH ::
  (OrdVia f a, U.Unbox a, PrimMonad m) =>
  BinaryHeap f (PrimState m) a ->
  a ->
  m ()
insertBH BinaryHeap {..} x = do
  size <- UM.unsafeRead intVarsBH _sizeBH
  UM.unsafeWrite intVarsBH _sizeBH (size + 1)
  UM.unsafeWrite internalVecBH size x
  siftUpBy (compareVia priorityBH) size internalVecBH
{-# INLINE insertBH #-}

unsafeDeleteBH_ ::
  (OrdVia f a, U.Unbox a, PrimMonad m) =>
  BinaryHeap f (PrimState m) a ->
  m ()
unsafeDeleteBH_ BinaryHeap {..} = do
  size' <- subtract 1 <$!> UM.unsafeRead intVarsBH _sizeBH
  UM.unsafeWrite intVarsBH _sizeBH size'
  UM.unsafeSwap internalVecBH 0 size'
  siftDownBy (compareVia priorityBH) 0 (UM.unsafeTake size' internalVecBH)
{-# INLINE unsafeDeleteBH_ #-}

unsafeDeleteBH ::
  (OrdVia f a, U.Unbox a, PrimMonad m) =>
  BinaryHeap f (PrimState m) a ->
  m a
unsafeDeleteBH bh = unsafeViewBH bh <* unsafeDeleteBH_ bh
{-# INLINE unsafeDeleteBH #-}

modifyBH ::
  (OrdVia f a, U.Unbox a, PrimMonad m) =>
  BinaryHeap f (PrimState m) a ->
  (a -> a) ->
  m ()
modifyBH BinaryHeap {..} f = do
  UM.unsafeModify internalVecBH f 0
  size <- UM.unsafeRead intVarsBH _sizeBH
  siftDownBy (compareVia priorityBH) 0 (UM.unsafeTake size internalVecBH)
{-# INLINE modifyBH #-}

deleteFindBH ::
  (OrdVia f a, U.Unbox a, PrimMonad m) =>
  BinaryHeap f (PrimState m) a ->
  m (Maybe a)
deleteFindBH bh = do
  size <- getBinaryHeapSize bh
  if size > 0
    then Just <$> unsafeDeleteBH bh
    else return Nothing
{-# INLINE deleteFindBH #-}

clearBH :: (PrimMonad m) => BinaryHeap f (PrimState m) a -> m ()
clearBH BinaryHeap {..} = UM.unsafeWrite intVarsBH 0 0

freezeInternalVecBH ::
  (U.Unbox a, PrimMonad m) =>
  BinaryHeap f (PrimState m) a ->
  m (U.Vector a)
freezeInternalVecBH BinaryHeap {..} = do
  size <- UM.unsafeRead intVarsBH _sizeBH
  U.unsafeFreeze (UM.unsafeTake size internalVecBH)
