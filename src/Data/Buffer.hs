{-# LANGUAGE RecordWildCards #-}

-- | [Data.Buffer](https://github.com/cojna/iota/blob/master/src/Data/Buffer.hs) taken from [cojna/iota](https://github.com/cojna/iota) (thanks!)
module Data.Buffer where

import Control.Applicative
import Control.Exception (assert)
import Control.Monad (void, when)
import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Control.Monad.ST
import Data.Ix
import Data.Maybe
import qualified Data.Vector.Generic.Mutable as GM
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
    (GM.unsafeRead bufferVars _bufferBackPos)
    (GM.unsafeRead bufferVars _bufferFrontPos)
{-# INLINE lengthBuffer #-}

-- | \(O(1)\)
nullBuffer :: (PrimMonad m) => Buffer (PrimState m) a -> m Bool
nullBuffer = fmap (== 0) . lengthBuffer
{-# INLINE nullBuffer #-}

-- | \(O(1)\)
clearBuffer :: (PrimMonad m) => Buffer (PrimState m) a -> m ()
clearBuffer Buffer {bufferVars, initialBufferPos} = stToPrim $ do
  GM.unsafeWrite bufferVars _bufferFrontPos initialBufferPos
  GM.unsafeWrite bufferVars _bufferBackPos initialBufferPos

-- | \(O(N)\)
freezeBuffer ::
  (U.Unbox a, PrimMonad m) =>
  Buffer (PrimState m) a ->
  m (U.Vector a)
freezeBuffer Buffer {bufferVars, internalBuffer} = stToPrim $ do
  f <- GM.unsafeRead bufferVars _bufferFrontPos
  b <- GM.unsafeRead bufferVars _bufferBackPos
  U.freeze $ GM.unsafeSlice f (b - f) internalBuffer

-- | \(O(1)\)
unsafeFreezeBuffer ::
  (U.Unbox a, PrimMonad m) =>
  Buffer (PrimState m) a ->
  m (U.Vector a)
unsafeFreezeBuffer Buffer {bufferVars, internalBuffer} = stToPrim $ do
  f <- GM.unsafeRead bufferVars _bufferFrontPos
  b <- GM.unsafeRead bufferVars _bufferBackPos
  U.unsafeFreeze $ GM.unsafeSlice f (b - f) internalBuffer

-- | \(O(N)\)
freezeInternalBuffer ::
  (U.Unbox a, PrimMonad m) =>
  Buffer (PrimState m) a ->
  m (U.Vector a)
freezeInternalBuffer Buffer {bufferVars, internalBuffer} = stToPrim $ do
  b <- GM.unsafeRead bufferVars _bufferBackPos
  U.freeze $ GM.unsafeSlice 0 b internalBuffer

-- | \(O(1)\)
unsafeFreezeInternalBuffer ::
  (U.Unbox a, PrimMonad m) =>
  Buffer (PrimState m) a ->
  m (U.Vector a)
unsafeFreezeInternalBuffer Buffer {bufferVars, internalBuffer} = stToPrim $ do
  b <- GM.unsafeRead bufferVars _bufferBackPos
  U.unsafeFreeze $ GM.unsafeSlice 0 b internalBuffer

-- | \(O(1)\)
popFront :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> m (Maybe a)
popFront Buffer {bufferVars, internalBuffer} = stToPrim $ do
  f <- GM.unsafeRead bufferVars _bufferFrontPos
  b <- GM.unsafeRead bufferVars _bufferBackPos
  if f < b
    then do
      GM.unsafeWrite bufferVars _bufferFrontPos (f + 1)
      pure <$> GM.unsafeRead internalBuffer f
    else return Nothing
{-# INLINE popFront #-}

popFront_ :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> m ()
popFront_ = void . popFront
{-# INLINE popFront_ #-}

-- | \(O(L)\) The popped vector is from left to the right order.
popFrontN :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> m (Maybe (U.Vector a))
popFrontN Buffer {bufferVars, internalBuffer} len = stToPrim $ do
  f <- GM.unsafeRead bufferVars _bufferFrontPos
  b <- GM.unsafeRead bufferVars _bufferBackPos
  if b - f >= len
    then do
      res <- U.freeze (GM.slice f len internalBuffer)
      GM.unsafeWrite bufferVars _bufferFrontPos (f + len)
      pure $ Just res
    else return Nothing
{-# INLINE popFrontN #-}

-- | \(O(L)\) The popped vector is from left to the right order.
popBackN :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> m (Maybe (U.Vector a))
popBackN Buffer {bufferVars, internalBuffer} len = stToPrim $ do
  f <- GM.unsafeRead bufferVars _bufferFrontPos
  b <- GM.unsafeRead bufferVars _bufferBackPos
  if b - f >= len
    then do
      res <- U.freeze (GM.slice (b - len) len internalBuffer)
      GM.unsafeWrite bufferVars _bufferBackPos (b - len)
      pure $ Just res
    else pure Nothing
{-# INLINE popBackN #-}

-- | \(O(1)\)
popFrontN_ :: (PrimMonad m) => Buffer (PrimState m) a -> Int -> m (Maybe ())
popFrontN_ Buffer {bufferVars} len = stToPrim $ do
  f <- GM.unsafeRead bufferVars _bufferFrontPos
  b <- GM.unsafeRead bufferVars _bufferBackPos
  if b - f >= len
    then do
      GM.unsafeWrite bufferVars _bufferFrontPos (f + len)
      pure $ Just ()
    else pure Nothing
{-# INLINE popFrontN_ #-}

-- | \(O(1)\)
popBackN_ :: (PrimMonad m) => Buffer (PrimState m) a -> Int -> m (Maybe ())
popBackN_ Buffer {bufferVars} len = stToPrim $ do
  f <- GM.unsafeRead bufferVars _bufferFrontPos
  b <- GM.unsafeRead bufferVars _bufferBackPos
  if b - f >= len
    then do
      GM.unsafeWrite bufferVars _bufferBackPos (b - len)
      pure $ Just ()
    else pure Nothing
{-# INLINE popBackN_ #-}

-- | \(O(1)\)
popBack :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> m (Maybe a)
popBack Buffer {bufferVars, internalBuffer} = stToPrim $ do
  f <- GM.unsafeRead bufferVars _bufferFrontPos
  b <- GM.unsafeRead bufferVars _bufferBackPos
  if f < b
    then do
      GM.unsafeWrite bufferVars _bufferBackPos (b - 1)
      pure <$> GM.unsafeRead internalBuffer (b - 1)
    else return Nothing
{-# INLINE popBack #-}

-- | \(O(1)\)
popBack_ :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> m ()
popBack_ = void . popBack
{-# INLINE popBack_ #-}

-- | \(O(1)\)
pushFront :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> a -> m ()
pushFront Buffer {bufferVars, internalBuffer} x = stToPrim $ do
  f <- GM.unsafeRead bufferVars _bufferFrontPos
  GM.unsafeWrite bufferVars _bufferFrontPos (f - 1)
  assert (f > 0) $ do
    GM.unsafeWrite internalBuffer (f - 1) x
{-# INLINE pushFront #-}

-- | \(O(1)\)
pushBack :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> a -> m ()
pushBack Buffer {bufferVars, internalBuffer, internalBufferSize} x = stToPrim $ do
  b <- GM.unsafeRead bufferVars _bufferBackPos
  GM.unsafeWrite bufferVars _bufferBackPos (b + 1)
  assert (b < internalBufferSize) $ do
    GM.unsafeWrite internalBuffer b x
{-# INLINE pushBack #-}

-- | \(O(K)\)
pushFronts ::
  (U.Unbox a, PrimMonad m) =>
  Buffer (PrimState m) a ->
  U.Vector a ->
  m ()
pushFronts Buffer {bufferVars, internalBuffer} vec = stToPrim $ do
  let n = U.length vec
  f <- GM.unsafeRead bufferVars _bufferFrontPos
  GM.unsafeWrite bufferVars _bufferFrontPos (f - n)
  assert (n <= f) $ do
    U.unsafeCopy (GM.unsafeSlice (f - n) n internalBuffer) vec
{-# INLINE pushFronts #-}

-- | \(O(K)\)
pushBacks ::
  (U.Unbox a, PrimMonad m) =>
  Buffer (PrimState m) a ->
  U.Vector a ->
  m ()
pushBacks Buffer {bufferVars, internalBuffer, internalBufferSize} vec = stToPrim $ do
  let n = U.length vec
  b <- GM.unsafeRead bufferVars _bufferBackPos
  GM.unsafeWrite bufferVars _bufferBackPos (b + n)
  assert (b + n - 1 < internalBufferSize) $ do
    U.unsafeCopy (GM.unsafeSlice b n internalBuffer) vec
{-# INLINE pushBacks #-}

-- | \(O(1)\)
readMaybeFront :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> m (Maybe a)
readMaybeFront Buffer {..} i = stToPrim $ do
  !f <- GM.unsafeRead bufferVars _bufferFrontPos
  !b <- GM.unsafeRead bufferVars _bufferBackPos
  if inRange (f, b - 1) (f + i)
    then Just <$> GM.read internalBuffer (f + i)
    else return Nothing
{-# INLINE readMaybeFront #-}

-- | \(O(1)\)
readMaybeBack :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> m (Maybe a)
readMaybeBack Buffer {..} i = stToPrim $ do
  !f <- GM.unsafeRead bufferVars _bufferFrontPos
  !b <- GM.unsafeRead bufferVars _bufferBackPos
  if inRange (f, b - 1) (b - 1 - i)
    then Just <$> GM.read internalBuffer (b - 1 - i)
    else return Nothing
{-# INLINE readMaybeBack #-}

-- | \(O(1)\)
readFront :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> m a
readFront buf i = fromMaybe (error ("readFront: " ++ show i)) <$> readMaybeFront buf i
{-# INLINE readFront #-}

-- | \(O(1)\)
readBack :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> m a
readBack buf i = fromMaybe (error ("readBack: " ++ show i)) <$> readMaybeBack buf i
{-# INLINE readBack #-}

_checkIndexBuffer :: (HasCallStack, PrimMonad m) => Buffer (PrimState m) a -> Int -> m ()
_checkIndexBuffer buf i = stToPrim $ do
  len <- lengthBuffer buf
  when (i < 0 || i >= len) $ do
    error $ "invalid index: " ++ show (0 :: Int, len - 1) ++ " " ++ show i
{-# INLINE _checkIndexBuffer #-}

-- | \(O(1)\)
writeFront :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> a -> m ()
writeFront buf i x = stToPrim $ do
  _checkIndexBuffer buf i
  unsafeWriteFront buf i x
{-# INLINE writeFront #-}

-- | \(O(1)\)
writeBack :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> a -> m ()
writeBack buf i x = stToPrim $ do
  _checkIndexBuffer buf i
  unsafeWriteBack buf i x
{-# INLINE writeBack #-}

-- | \(O(1)\)
unsafeWriteFront :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> a -> m ()
unsafeWriteFront Buffer {..} i x = stToPrim $ do
  f <- GM.unsafeRead bufferVars _bufferFrontPos
  GM.unsafeWrite internalBuffer (f + i) x
{-# INLINE unsafeWriteFront #-}

-- | \(O(1)\)
unsafeWriteBack :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> a -> m ()
unsafeWriteBack Buffer {..} i x = stToPrim $ do
  b <- GM.unsafeRead bufferVars _bufferBackPos
  GM.unsafeWrite internalBuffer (b - i) x
{-# INLINE unsafeWriteBack #-}

-- | \(O(1)\)
swapFront :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> Int -> m ()
swapFront buf i1 i2 = stToPrim $ do
  _checkIndexBuffer buf i1
  _checkIndexBuffer buf i2
  unsafeSwapFront buf i1 i2
{-# INLINE swapFront #-}

-- | \(O(1)\)
swapBack :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> Int -> m ()
swapBack buf i1 i2 = stToPrim $ do
  _checkIndexBuffer buf i1
  _checkIndexBuffer buf i2
  unsafeSwapBack buf i1 i2
{-# INLINE swapBack #-}

-- | \(O(1)\)
unsafeSwapFront :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> Int -> m ()
unsafeSwapFront Buffer {..} i1 i2 = stToPrim $ do
  f <- GM.unsafeRead bufferVars _bufferFrontPos
  GM.unsafeSwap internalBuffer (f + i1) (f + i2)
{-# INLINE unsafeSwapFront #-}

-- | \(O(1)\)
unsafeSwapBack :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> Int -> m ()
unsafeSwapBack Buffer {..} i1 i2 = stToPrim $ do
  b <- GM.unsafeRead bufferVars _bufferBackPos
  GM.unsafeSwap internalBuffer (b - i1) (b - i2)
{-# INLINE unsafeSwapBack #-}

-- | \(O(1)\)
modifyFront :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> (a -> a) -> Int -> m ()
modifyFront buf m i = stToPrim $ do
  _checkIndexBuffer buf i
  unsafeModifyFront buf m i
{-# INLINE modifyFront #-}

-- | \(O(1)\)
modifyBack :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> (a -> a) -> Int -> m ()
modifyBack buf m i = stToPrim $ do
  _checkIndexBuffer buf i
  unsafeModifyBack buf m i
{-# INLINE modifyBack #-}

-- | \(O(1)\)
unsafeModifyFront :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> (a -> a) -> Int -> m ()
unsafeModifyFront Buffer {..} m i = stToPrim $ do
  f <- GM.unsafeRead bufferVars _bufferFrontPos
  GM.unsafeModify internalBuffer m (f + i)
{-# INLINE unsafeModifyFront #-}

-- | \(O(1)\)
unsafeModifyBack :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> (a -> a) -> Int -> m ()
unsafeModifyBack Buffer {..} m i = stToPrim $ do
  b <- GM.unsafeRead bufferVars _bufferBackPos
  GM.unsafeModify internalBuffer m (b - i)
{-# INLINE unsafeModifyBack #-}

-- | \(O(1)\)
modifyMFront :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> (a -> m a) -> Int -> m ()
modifyMFront buf m i = do
  _checkIndexBuffer buf i
  unsafeModifyMFront buf m i
{-# INLINE modifyMFront #-}

-- | \(O(1)\)
modifyMBack :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> (a -> m a) -> Int -> m ()
modifyMBack buf m i = do
  _checkIndexBuffer buf i
  unsafeModifyMBack buf m i
{-# INLINE modifyMBack #-}

-- | \(O(1)\)
unsafeModifyMFront :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> (a -> m a) -> Int -> m ()
unsafeModifyMFront Buffer {..} m i = do
  f <- GM.unsafeRead bufferVars _bufferFrontPos
  GM.unsafeModifyM internalBuffer m (f + i)
{-# INLINE unsafeModifyMFront #-}

-- | \(O(1)\)
unsafeModifyMBack :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> (a -> m a) -> Int -> m ()
unsafeModifyMBack Buffer {..} m i = do
  b <- GM.unsafeRead bufferVars _bufferBackPos
  GM.unsafeModifyM internalBuffer m (b - i)
{-# INLINE unsafeModifyMBack #-}

-- | \(O(1)\)
exchangeFront :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> a -> m a
exchangeFront buf i x = stToPrim $ do
  _checkIndexBuffer buf i
  unsafeExchangeFront buf i x
{-# INLINE exchangeFront #-}

-- | \(O(1)\)
exchangeBack :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> a -> m a
exchangeBack buf i x = stToPrim $ do
  _checkIndexBuffer buf i
  unsafeExchangeBack buf i x
{-# INLINE exchangeBack #-}

-- | \(O(1)\)
unsafeExchangeFront :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> a -> m a
unsafeExchangeFront Buffer {..} i x = stToPrim $ do
  f <- GM.unsafeRead bufferVars _bufferFrontPos
  GM.unsafeExchange internalBuffer (f + i) x
{-# INLINE unsafeExchangeFront #-}

-- | \(O(1)\)
unsafeExchangeBack :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> a -> m a
unsafeExchangeBack Buffer {..} i x = stToPrim $ do
  b <- GM.unsafeRead bufferVars _bufferBackPos
  GM.unsafeExchange internalBuffer (b - i) x
{-# INLINE unsafeExchangeBack #-}

-- | \(O(N)\)
cloneBuffer :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> m (Buffer (PrimState m) a)
cloneBuffer Buffer {..} = stToPrim $ do
  vars' <- GM.clone bufferVars
  buf' <- GM.clone internalBuffer
  return $ Buffer vars' initialBufferPos buf' internalBufferSize
