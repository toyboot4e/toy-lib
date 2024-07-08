{-# LANGUAGE RecordWildCards #-}

-- | [Data.Buffer](https://github.com/cojna/iota/blob/master/src/Data/Buffer.hs) taken from [cojna/iota](https://github.com/cojna/iota) (thanks!)
module Data.Buffer where

import Control.Applicative
import Control.Exception (assert)
import Control.Monad (void, when)
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Ix
import Data.Maybe
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Stack (HasCallStack)

data Buffer s a = Buffer
  { -- | Stores the @[front, back)@ position of the buffer in use. The @front@ value is zero when
    -- the buffer is initialized as a stack or queue. The front value is at the middle of the
    -- internal buffer when initialized as a deque.
    bufferVars :: !(UM.MVector s Int),
    -- | `bufferVars` initial values. Used in `clearBuffer`.
    initialBufferPos :: !Int,
    -- | The storage.
    internalBuffer :: !(UM.MVector s a),
    -- | The capacity of the buffer. It's doubled when initialized as a dequeue.
    internalBufferSize :: !Int
  }

_bufferFrontPos :: Int
_bufferFrontPos = 0

_bufferBackPos :: Int
_bufferBackPos = 1

-- | \(O(N)\) Creates a buffer of length @n@ with initial value at @zero@.
newBuffer :: (U.Unbox a, PrimMonad m) => Int -> m (Buffer (PrimState m) a)
newBuffer n = Buffer <$> UM.replicate 2 0 <*> return 0 <*> UM.unsafeNew n <*> pure n

-- | \(O(N)\) Creates a buffer of length @n@ with initial value at @n - 1@.
newRevBuffer :: (U.Unbox a, PrimMonad m) => Int -> m (Buffer (PrimState m) a)
newRevBuffer n = Buffer <$> UM.replicate 2 (n - 1) <*> return (n - 1) <*> UM.unsafeNew n <*> pure n

type Deque s a = Buffer s a

-- | \(O(N)\) Creates a buffer of length @2 * n@ with initial value at @n@.
newBufferAsDeque :: (U.Unbox a, PrimMonad m) => Int -> m (Buffer (PrimState m) a)
newBufferAsDeque n =
  Buffer
    <$> UM.replicate 2 n
    <*> pure n
    <*> UM.unsafeNew (2 * n)
    <*> pure (2 * n)

-- | \(O(N)\) Freezes a buffer after creation.
createBuffer :: (U.Unbox a) => (forall s. ST s (Buffer s a)) -> U.Vector a
createBuffer f = runST $ do
  !buf <- f
  unsafeFreezeBuffer buf

-- | \(O(1)\)
lengthBuffer :: (PrimMonad m) => Buffer (PrimState m) a -> m Int
lengthBuffer Buffer {bufferVars} =
  liftA2
    (-)
    (UM.unsafeRead bufferVars _bufferBackPos)
    (UM.unsafeRead bufferVars _bufferFrontPos)
{-# INLINE lengthBuffer #-}

-- | \(O(1)\)
nullBuffer :: (PrimMonad m) => Buffer (PrimState m) a -> m Bool
nullBuffer = fmap (== 0) . lengthBuffer
{-# INLINE nullBuffer #-}

-- | \(O(1)\)
clearBuffer :: (PrimMonad m) => Buffer (PrimState m) a -> m ()
clearBuffer Buffer {bufferVars, initialBufferPos} = do
  UM.unsafeWrite bufferVars _bufferFrontPos initialBufferPos
  UM.unsafeWrite bufferVars _bufferBackPos initialBufferPos

-- | \(O(N)\)
freezeBuffer ::
  (U.Unbox a, PrimMonad m) =>
  Buffer (PrimState m) a ->
  m (U.Vector a)
freezeBuffer Buffer {bufferVars, internalBuffer} = do
  f <- UM.unsafeRead bufferVars _bufferFrontPos
  b <- UM.unsafeRead bufferVars _bufferBackPos
  U.freeze $ UM.unsafeSlice f (b - f) internalBuffer

-- | \(O(1)\)
unsafeFreezeBuffer ::
  (U.Unbox a, PrimMonad m) =>
  Buffer (PrimState m) a ->
  m (U.Vector a)
unsafeFreezeBuffer Buffer {bufferVars, internalBuffer} = do
  f <- UM.unsafeRead bufferVars _bufferFrontPos
  b <- UM.unsafeRead bufferVars _bufferBackPos
  U.unsafeFreeze $ UM.unsafeSlice f (b - f) internalBuffer

-- | \(O(N)\)
freezeInternalBuffer ::
  (U.Unbox a, PrimMonad m) =>
  Buffer (PrimState m) a ->
  m (U.Vector a)
freezeInternalBuffer Buffer {bufferVars, internalBuffer} = do
  b <- UM.unsafeRead bufferVars _bufferBackPos
  U.freeze $ UM.unsafeSlice 0 b internalBuffer

-- | \(O(1)\)
unsafeFreezeInternalBuffer ::
  (U.Unbox a, PrimMonad m) =>
  Buffer (PrimState m) a ->
  m (U.Vector a)
unsafeFreezeInternalBuffer Buffer {bufferVars, internalBuffer} = do
  b <- UM.unsafeRead bufferVars _bufferBackPos
  U.unsafeFreeze $ UM.unsafeSlice 0 b internalBuffer

-- | \(O(1)\)
popFront :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> m (Maybe a)
popFront Buffer {bufferVars, internalBuffer} = do
  f <- UM.unsafeRead bufferVars _bufferFrontPos
  b <- UM.unsafeRead bufferVars _bufferBackPos
  if f < b
    then do
      UM.unsafeWrite bufferVars _bufferFrontPos (f + 1)
      pure <$> UM.unsafeRead internalBuffer f
    else return Nothing
{-# INLINE popFront #-}

popFront_ :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> m ()
popFront_ = void . popFront
{-# INLINE popFront_ #-}

-- | \(O(L)\) The popped vector is from left to the right order.
popFrontN :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> m (Maybe (U.Vector a))
popFrontN Buffer {bufferVars, internalBuffer} len = do
  f <- UM.unsafeRead bufferVars _bufferFrontPos
  b <- UM.unsafeRead bufferVars _bufferBackPos
  if b - f >= len
    then do
      res <- U.freeze (UM.slice f len internalBuffer)
      UM.unsafeWrite bufferVars _bufferFrontPos (f + len)
      pure $ Just res
    else return Nothing
{-# INLINE popFrontN #-}

-- | \(O(L)\) The popped vector is from left to the right order.
popBackN :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> m (Maybe (U.Vector a))
popBackN Buffer {bufferVars, internalBuffer} len = do
  f <- UM.unsafeRead bufferVars _bufferFrontPos
  b <- UM.unsafeRead bufferVars _bufferBackPos
  if b - f >= len
    then do
      res <- U.freeze (UM.slice (b - len) len internalBuffer)
      UM.unsafeWrite bufferVars _bufferBackPos (b - len)
      pure $ Just res
    else pure Nothing
{-# INLINE popBackN #-}

-- | \(O(1)\)
popFrontN_ :: (PrimMonad m) => Buffer (PrimState m) a -> Int -> m (Maybe ())
popFrontN_ Buffer {bufferVars} len = do
  f <- UM.unsafeRead bufferVars _bufferFrontPos
  b <- UM.unsafeRead bufferVars _bufferBackPos
  if b - f >= len
    then do
      UM.unsafeWrite bufferVars _bufferFrontPos (f + len)
      pure $ Just ()
    else pure Nothing
{-# INLINE popFrontN_ #-}

-- | \(O(1)\)
popBackN_ :: (PrimMonad m) => Buffer (PrimState m) a -> Int -> m (Maybe ())
popBackN_ Buffer {bufferVars} len = do
  f <- UM.unsafeRead bufferVars _bufferFrontPos
  b <- UM.unsafeRead bufferVars _bufferBackPos
  if b - f >= len
    then do
      UM.unsafeWrite bufferVars _bufferBackPos (b - len)
      pure $ Just ()
    else pure Nothing
{-# INLINE popBackN_ #-}

-- | \(O(1)\)
popBack :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> m (Maybe a)
popBack Buffer {bufferVars, internalBuffer} = do
  f <- UM.unsafeRead bufferVars _bufferFrontPos
  b <- UM.unsafeRead bufferVars _bufferBackPos
  if f < b
    then do
      UM.unsafeWrite bufferVars _bufferBackPos (b - 1)
      pure <$> UM.unsafeRead internalBuffer (b - 1)
    else return Nothing
{-# INLINE popBack #-}

-- | \(O(1)\)
popBack_ :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> m ()
popBack_ = void . popBack
{-# INLINE popBack_ #-}

-- | \(O(1)\)
pushFront :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> a -> m ()
pushFront Buffer {bufferVars, internalBuffer} x = do
  f <- UM.unsafeRead bufferVars _bufferFrontPos
  UM.unsafeWrite bufferVars _bufferFrontPos (f - 1)
  assert (f > 0) $ do
    UM.unsafeWrite internalBuffer (f - 1) x
{-# INLINE pushFront #-}

-- | \(O(1)\)
pushBack :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> a -> m ()
pushBack Buffer {bufferVars, internalBuffer, internalBufferSize} x = do
  b <- UM.unsafeRead bufferVars _bufferBackPos
  UM.unsafeWrite bufferVars _bufferBackPos (b + 1)
  assert (b < internalBufferSize) $ do
    UM.unsafeWrite internalBuffer b x
{-# INLINE pushBack #-}

-- | \(O(K)\)
pushFronts ::
  (U.Unbox a, PrimMonad m) =>
  Buffer (PrimState m) a ->
  U.Vector a ->
  m ()
pushFronts Buffer {bufferVars, internalBuffer} vec = do
  let n = U.length vec
  f <- UM.unsafeRead bufferVars _bufferFrontPos
  UM.unsafeWrite bufferVars _bufferFrontPos (f - n)
  assert (n <= f) $ do
    U.unsafeCopy (UM.unsafeSlice (f - n) n internalBuffer) vec
{-# INLINE pushFronts #-}

-- | \(O(K)\)
pushBacks ::
  (U.Unbox a, PrimMonad m) =>
  Buffer (PrimState m) a ->
  U.Vector a ->
  m ()
pushBacks Buffer {bufferVars, internalBuffer, internalBufferSize} vec = do
  let n = U.length vec
  b <- UM.unsafeRead bufferVars _bufferBackPos
  UM.unsafeWrite bufferVars _bufferBackPos (b + n)
  assert (b + n - 1 < internalBufferSize) $ do
    U.unsafeCopy (UM.unsafeSlice b n internalBuffer) vec
{-# INLINE pushBacks #-}

-- | \(O(1)\)
viewFrontN :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> m (Maybe a)
viewFrontN Buffer {..} i = do
  !f <- UM.unsafeRead bufferVars _bufferFrontPos
  !b <- UM.unsafeRead bufferVars _bufferBackPos
  if inRange (f, b - 1) (f + i)
    then Just <$> UM.read internalBuffer (f + i)
    else return Nothing
{-# INLINE viewFrontN #-}

-- | \(O(1)\)
viewBackN :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> m (Maybe a)
viewBackN Buffer {..} i = do
  !f <- UM.unsafeRead bufferVars _bufferFrontPos
  !b <- UM.unsafeRead bufferVars _bufferBackPos
  if inRange (f, b - 1) (b - 1 - i)
    then Just <$> UM.read internalBuffer (b - 1 - i)
    else return Nothing
{-# INLINE viewBackN #-}

-- | \(O(1)\)
viewFront :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> m (Maybe a)
viewFront buf = viewFrontN buf 0
{-# INLINE viewFront #-}

-- | \(O(1)\)
viewBack :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> m (Maybe a)
viewBack buf = viewBackN buf 0
{-# INLINE viewBack #-}

-- | \(O(1)\)
readFront :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> m a
readFront buf i = fromMaybe (error ("readFront: " ++ show i)) <$> viewFrontN buf i
{-# INLINE readFront #-}

-- | \(O(1)\)
readBack :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> m a
readBack buf i = fromMaybe (error ("readBack: " ++ show i)) <$> viewBackN buf i
{-# INLINE readBack #-}

_checkIndexBuffer :: (HasCallStack, PrimMonad m) => Buffer (PrimState m) a -> Int -> m ()
_checkIndexBuffer buf i = do
  len <- lengthBuffer buf
  when (i < 0 || i >= len) $ do
    error $ "invalid index: " ++ show (0 :: Int, len - 1) ++ " " ++ show i
{-# INLINE _checkIndexBuffer #-}

-- | \(O(1)\)
writeFront :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> a -> m ()
writeFront buf@Buffer {..} i x = do
  _checkIndexBuffer buf i
  f <- UM.unsafeRead bufferVars _bufferFrontPos
  UM.unsafeWrite internalBuffer (f + i) x
{-# INLINE writeFront #-}

-- | \(O(1)\)
writeBack :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> a -> m ()
writeBack buf@Buffer {..} i x = do
  _checkIndexBuffer buf i
  b <- UM.unsafeRead bufferVars _bufferBackPos
  UM.unsafeWrite internalBuffer (b - i) x
{-# INLINE writeBack #-}

-- | \(O(1)\)
swapFront :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> Int -> m ()
swapFront buf@Buffer {..} i1 i2 = do
  _checkIndexBuffer buf i1
  _checkIndexBuffer buf i2
  f <- UM.unsafeRead bufferVars _bufferFrontPos
  UM.unsafeSwap internalBuffer (f + i1) (f + i2)
{-# INLINE swapFront #-}

-- | \(O(1)\)
swapBack :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> Int -> m ()
swapBack buf@Buffer {..} i1 i2 = do
  _checkIndexBuffer buf i1
  _checkIndexBuffer buf i2
  b <- UM.unsafeRead bufferVars _bufferBackPos
  UM.unsafeSwap internalBuffer (b - i1) (b - i2)
{-# INLINE swapBack #-}

-- | \(O(1)\)
modifyFront :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> (a -> a) -> Int -> m ()
modifyFront buf@Buffer {..} m i = do
  _checkIndexBuffer buf i
  f <- UM.unsafeRead bufferVars _bufferFrontPos
  UM.unsafeModify internalBuffer m (f + i)
{-# INLINE modifyFront #-}

-- | \(O(1)\)
modifyBack :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> (a -> a) -> Int -> m ()
modifyBack buf@Buffer {..} m i = do
  _checkIndexBuffer buf i
  b <- UM.unsafeRead bufferVars _bufferBackPos
  UM.unsafeModify internalBuffer m (b - i)
{-# INLINE modifyBack #-}

-- | \(O(1)\)
modifyMFront :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> (a -> m a) -> Int -> m ()
modifyMFront buf@Buffer {..} m i = do
  _checkIndexBuffer buf i
  f <- UM.unsafeRead bufferVars _bufferFrontPos
  UM.unsafeModifyM internalBuffer m (f + i)
{-# INLINE modifyMFront #-}

-- | \(O(1)\)
modifyMBack :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> (a -> m a) -> Int -> m ()
modifyMBack buf@Buffer {..} m i = do
  _checkIndexBuffer buf i
  b <- UM.unsafeRead bufferVars _bufferBackPos
  UM.unsafeModifyM internalBuffer m (b - i)
{-# INLINE modifyMBack #-}

-- | \(O(1)\)
exchangeFront :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> a -> m a
exchangeFront buf@Buffer {..} i x = do
  _checkIndexBuffer buf i
  f <- UM.unsafeRead bufferVars _bufferFrontPos
  UM.unsafeExchange internalBuffer (f + i) x
{-# INLINE exchangeFront #-}

-- | \(O(1)\)
exchangeBack :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> a -> m a
exchangeBack buf@Buffer {..} i x = do
  _checkIndexBuffer buf i
  b <- UM.unsafeRead bufferVars _bufferBackPos
  UM.unsafeExchange internalBuffer (b - i) x
{-# INLINE exchangeBack #-}

-- | \(O(N)\)
cloneBuffer :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> m (Buffer (PrimState m) a)
cloneBuffer Buffer {..} = do
  vars' <- UM.clone bufferVars
  buf' <- UM.clone internalBuffer
  return $ Buffer vars' initialBufferPos buf' internalBufferSize
