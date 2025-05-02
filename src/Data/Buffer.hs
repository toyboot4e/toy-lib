{-# LANGUAGE RecordWildCards #-}

-- | [Data.Buffer](https://github.com/cojna/iota/blob/master/src/Data/Buffer.hs) taken from [cojna/iota](https://github.com/cojna/iota) (thanks!)
-- It's a fixed-sized mutable vector with push/pop API.
module Data.Buffer where

import Control.Applicative
import Control.Monad (void, when)
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Ix
import Data.Maybe
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Stack (HasCallStack)
import ToyLib.Debug (asserted)

-- | A fixed-sized mutable vector with push/pop API.
data Buffer s a = Buffer
  { -- | Stores the @[front, back)@ position of the buffer in use. The @front@ value is zero when
    -- the buffer is initialized as a stack or queue. The front value is at the middle of the
    -- internal buffer when initialized as a deque.
    bufferVars :: !(UM.MVector s Int),
    -- | `bufferVars` initial values. Used in `clearBuffer`.
    initialBufferPos :: {-# UNPACK #-} !Int,
    -- | The storage.
    internalBuffer :: !(UM.MVector s a),
    -- | The capacity of the buffer. It's doubled when initialized as a dequeue.
    internalBufferSize :: {-# UNPACK #-} !Int
  }

_bufferFrontPos :: Int
_bufferFrontPos = 0

_bufferBackPos :: Int
_bufferBackPos = 1

-- | \(O(N)\) Creates a buffer of length @n@ with initial value at @zero@. This is mostlly for
-- queues.
newBuffer :: (U.Unbox a, PrimMonad m) => Int -> m (Buffer (PrimState m) a)
newBuffer n = Buffer <$> UM.replicate 2 0 <*> pure 0 <*> UM.unsafeNew n <*> pure n

-- | \(O(N)\) Creates a buffer of length @n@ with initial value at @n - 1@. This is mostly for
-- stacks.
newRevBuffer :: (U.Unbox a, PrimMonad m) => Int -> m (Buffer (PrimState m) a)
newRevBuffer n = Buffer <$> UM.replicate 2 (n - 1) <*> pure (n - 1) <*> UM.unsafeNew n <*> pure n

-- | \(O(N)\) Wraps a mutable vector with push/pop API.
{-# INLINE buildBuffer #-}
buildBuffer :: (U.Unbox a, PrimMonad m) => UM.MVector (PrimState m) a -> m (Buffer (PrimState m) a)
buildBuffer internalBuffer = do
  let !n = GM.length internalBuffer
  Buffer <$> UM.generate 2 (* n) <*> pure 0 <*> pure internalBuffer <*> pure n

-- | \(O(N)\) Creates a mutable vector with push/pop API.
{-# INLINE generateBuffer #-}
generateBuffer :: (U.Unbox a, PrimMonad m) => Int -> (Int -> a) -> m (Buffer (PrimState m) a)
generateBuffer n f = buildBuffer =<< UM.generate n f

-- | Alias to `Buffer` creates with `newBufferAsDeuque`.
type Deque s a = Buffer s a

-- | \(O(N)\) Creates a buffer of length @2 * n@ with initial value at @n@.
newBufferAsDeque :: (U.Unbox a, PrimMonad m) => Int -> m (Buffer (PrimState m) a)
newBufferAsDeque n =
  Buffer
    <$> UM.replicate 2 n
    <*> pure n
    <*> UM.unsafeNew (2 * n)
    <*> pure (2 * n)

-- | \(O(N)\) Freezes a buffer after use.
{-# INLINE createBuffer #-}
createBuffer :: (U.Unbox a) => (forall s. ST s (Buffer s a)) -> U.Vector a
createBuffer f = runST $ do
  !buf <- f
  unsafeFreezeBuffer buf

-- | \(O(1)\)
{-# INLINE lengthBuffer #-}
lengthBuffer :: (PrimMonad m) => Buffer (PrimState m) a -> m Int
lengthBuffer Buffer {bufferVars} =
  liftA2
    (-)
    (GM.unsafeRead bufferVars _bufferBackPos)
    (GM.unsafeRead bufferVars _bufferFrontPos)

-- | \(O(1)\)
{-# INLINE nullBuffer #-}
nullBuffer :: (PrimMonad m) => Buffer (PrimState m) a -> m Bool
nullBuffer = fmap (== 0) . lengthBuffer

-- | \(O(1)\)
clearBuffer :: (PrimMonad m) => Buffer (PrimState m) a -> m ()
clearBuffer Buffer {bufferVars, initialBufferPos} = do
  GM.unsafeWrite bufferVars _bufferFrontPos initialBufferPos
  GM.unsafeWrite bufferVars _bufferBackPos initialBufferPos

-- | \(O(N)\)
freezeBuffer ::
  (U.Unbox a, PrimMonad m) =>
  Buffer (PrimState m) a ->
  m (U.Vector a)
freezeBuffer Buffer {bufferVars, internalBuffer} = do
  f <- GM.unsafeRead bufferVars _bufferFrontPos
  b <- GM.unsafeRead bufferVars _bufferBackPos
  U.freeze $ GM.unsafeSlice f (b - f) internalBuffer

-- | \(O(1)\)
unsafeFreezeBuffer ::
  (U.Unbox a, PrimMonad m) =>
  Buffer (PrimState m) a ->
  m (U.Vector a)
unsafeFreezeBuffer Buffer {bufferVars, internalBuffer} = do
  f <- GM.unsafeRead bufferVars _bufferFrontPos
  b <- GM.unsafeRead bufferVars _bufferBackPos
  U.unsafeFreeze $ GM.unsafeSlice f (b - f) internalBuffer

-- | \(O(N)\)
freezeInternalBuffer ::
  (U.Unbox a, PrimMonad m) =>
  Buffer (PrimState m) a ->
  m (U.Vector a)
freezeInternalBuffer Buffer {bufferVars, internalBuffer} = do
  b <- GM.unsafeRead bufferVars _bufferBackPos
  U.freeze $ GM.unsafeSlice 0 b internalBuffer

-- | \(O(1)\)
unsafeFreezeInternalBuffer ::
  (U.Unbox a, PrimMonad m) =>
  Buffer (PrimState m) a ->
  m (U.Vector a)
unsafeFreezeInternalBuffer Buffer {bufferVars, internalBuffer} = do
  b <- GM.unsafeRead bufferVars _bufferBackPos
  U.unsafeFreeze $ GM.unsafeSlice 0 b internalBuffer

-- | \(O(1)\)
{-# INLINE popFront #-}
popFront :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> m (Maybe a)
popFront Buffer {bufferVars, internalBuffer} = do
  f <- GM.unsafeRead bufferVars _bufferFrontPos
  b <- GM.unsafeRead bufferVars _bufferBackPos
  if f < b
    then do
      GM.unsafeWrite bufferVars _bufferFrontPos (f + 1)
      pure <$> GM.unsafeRead internalBuffer f
    else pure Nothing

{-# INLINE popFront_ #-}
popFront_ :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> m ()
popFront_ = void . popFront

-- | \(O(L)\) The popped vector is from left to the right order.
{-# INLINE popFrontN #-}
popFrontN :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> m (Maybe (U.Vector a))
popFrontN Buffer {bufferVars, internalBuffer} len = do
  f <- GM.unsafeRead bufferVars _bufferFrontPos
  b <- GM.unsafeRead bufferVars _bufferBackPos
  if b - f >= len
    then do
      res <- U.freeze (GM.slice f len internalBuffer)
      GM.unsafeWrite bufferVars _bufferFrontPos (f + len)
      pure $ Just res
    else pure Nothing

-- | \(O(L)\) The popped vector is from left to the right order.
{-# INLINE popBackN #-}
popBackN :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> m (Maybe (U.Vector a))
popBackN Buffer {bufferVars, internalBuffer} len = do
  f <- GM.unsafeRead bufferVars _bufferFrontPos
  b <- GM.unsafeRead bufferVars _bufferBackPos
  if b - f >= len
    then do
      res <- U.freeze (GM.slice (b - len) len internalBuffer)
      GM.unsafeWrite bufferVars _bufferBackPos (b - len)
      pure $ Just res
    else pure Nothing

-- | \(O(1)\)
{-# INLINE popFrontN_ #-}
popFrontN_ :: (PrimMonad m) => Buffer (PrimState m) a -> Int -> m (Maybe ())
popFrontN_ Buffer {bufferVars} len = do
  f <- GM.unsafeRead bufferVars _bufferFrontPos
  b <- GM.unsafeRead bufferVars _bufferBackPos
  if b - f >= len
    then do
      GM.unsafeWrite bufferVars _bufferFrontPos (f + len)
      pure $ Just ()
    else pure Nothing

-- | \(O(1)\)
{-# INLINE popBackN_ #-}
popBackN_ :: (PrimMonad m) => Buffer (PrimState m) a -> Int -> m (Maybe ())
popBackN_ Buffer {bufferVars} len = do
  f <- GM.unsafeRead bufferVars _bufferFrontPos
  b <- GM.unsafeRead bufferVars _bufferBackPos
  if b - f >= len
    then do
      GM.unsafeWrite bufferVars _bufferBackPos (b - len)
      pure $ Just ()
    else pure Nothing

-- | \(O(1)\)
{-# INLINE popBack #-}
popBack :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> m (Maybe a)
popBack Buffer {bufferVars, internalBuffer} = do
  f <- GM.unsafeRead bufferVars _bufferFrontPos
  b <- GM.unsafeRead bufferVars _bufferBackPos
  if f < b
    then do
      GM.unsafeWrite bufferVars _bufferBackPos (b - 1)
      pure <$> GM.unsafeRead internalBuffer (b - 1)
    else pure Nothing

-- | \(O(1)\)
{-# INLINE popBack_ #-}
popBack_ :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> m ()
popBack_ = void . popBack

-- | \(O(1)\)
{-# INLINE pushFront #-}
pushFront :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> a -> m ()
pushFront Buffer {bufferVars, internalBuffer} x = do
  f <- GM.unsafeRead bufferVars _bufferFrontPos
  GM.unsafeWrite bufferVars _bufferFrontPos (f - 1)
  asserted (f > 0) $ do
    GM.unsafeWrite internalBuffer (f - 1) x

-- | \(O(1)\)
{-# INLINE pushBack #-}
pushBack :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> a -> m ()
pushBack Buffer {bufferVars, internalBuffer, internalBufferSize} x = do
  b <- GM.unsafeRead bufferVars _bufferBackPos
  GM.unsafeWrite bufferVars _bufferBackPos (b + 1)
  asserted (b < internalBufferSize) $ do
    GM.unsafeWrite internalBuffer b x

-- | \(O(K)\)
{-# INLINE pushFronts #-}
pushFronts ::
  (HasCallStack, U.Unbox a, PrimMonad m) =>
  Buffer (PrimState m) a ->
  U.Vector a ->
  m ()
pushFronts Buffer {bufferVars, internalBuffer} vec = do
  let n = U.length vec
  f <- GM.unsafeRead bufferVars _bufferFrontPos
  GM.unsafeWrite bufferVars _bufferFrontPos (f - n)
  asserted (n <= f) $ do
    U.unsafeCopy (GM.unsafeSlice (f - n) n internalBuffer) vec

-- | \(O(K)\)
{-# INLINE pushBacks #-}
pushBacks ::
  (HasCallStack, U.Unbox a, PrimMonad m) =>
  Buffer (PrimState m) a ->
  U.Vector a ->
  m ()
pushBacks Buffer {bufferVars, internalBuffer, internalBufferSize} vec = do
  let n = U.length vec
  b <- GM.unsafeRead bufferVars _bufferBackPos
  GM.unsafeWrite bufferVars _bufferBackPos (b + n)
  asserted (b + n - 1 < internalBufferSize) $ do
    U.unsafeCopy (GM.unsafeSlice b n internalBuffer) vec

-- | \(O(1)\)
{-# INLINE readMaybeFront #-}
readMaybeFront :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> m (Maybe a)
readMaybeFront Buffer {..} i = do
  !f <- GM.unsafeRead bufferVars _bufferFrontPos
  !b <- GM.unsafeRead bufferVars _bufferBackPos
  if inRange (f, b - 1) (f + i)
    then Just <$> GM.read internalBuffer (f + i)
    else pure Nothing

-- | \(O(1)\)
{-# INLINE readMaybeBack #-}
readMaybeBack :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> m (Maybe a)
readMaybeBack Buffer {..} i = do
  !f <- GM.unsafeRead bufferVars _bufferFrontPos
  !b <- GM.unsafeRead bufferVars _bufferBackPos
  if inRange (f, b - 1) (b - 1 - i)
    then Just <$> GM.read internalBuffer (b - 1 - i)
    else pure Nothing

-- | \(O(1)\)
{-# INLINE readFront #-}
readFront :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> m a
readFront buf i = fromMaybe (error ("readFront: " ++ show i)) <$> readMaybeFront buf i

-- | \(O(1)\)
{-# INLINE readBack #-}
readBack :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> m a
readBack buf i = fromMaybe (error ("readBack: " ++ show i)) <$> readMaybeBack buf i

{-# INLINE _checkIndexBuffer #-}
_checkIndexBuffer :: (HasCallStack, PrimMonad m) => Buffer (PrimState m) a -> Int -> m ()
_checkIndexBuffer buf i = do
  len <- lengthBuffer buf
  when (i < 0 || i >= len) $ do
    error $ "invalid index: " ++ show (0 :: Int, len - 1) ++ " " ++ show i

-- | \(O(1)\)
{-# INLINE writeFront #-}
writeFront :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> a -> m ()
writeFront buf i x = do
  _checkIndexBuffer buf i
  unsafeWriteFront buf i x

-- | \(O(1)\)
{-# INLINE writeBack #-}
writeBack :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> a -> m ()
writeBack buf i x = do
  _checkIndexBuffer buf i
  unsafeWriteBack buf i x

-- | \(O(1)\)
{-# INLINE unsafeWriteFront #-}
unsafeWriteFront :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> a -> m ()
unsafeWriteFront Buffer {..} i x = do
  f <- GM.unsafeRead bufferVars _bufferFrontPos
  GM.unsafeWrite internalBuffer (f + i) x

-- | \(O(1)\)
{-# INLINE unsafeWriteBack #-}
unsafeWriteBack :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> a -> m ()
unsafeWriteBack Buffer {..} i x = do
  b <- GM.unsafeRead bufferVars _bufferBackPos
  GM.unsafeWrite internalBuffer (b - i) x

-- | \(O(1)\)
{-# INLINE swapFront #-}
swapFront :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> Int -> m ()
swapFront buf i1 i2 = do
  _checkIndexBuffer buf i1
  _checkIndexBuffer buf i2
  unsafeSwapFront buf i1 i2

-- | \(O(1)\)
{-# INLINE swapBack #-}
swapBack :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> Int -> m ()
swapBack buf i1 i2 = do
  _checkIndexBuffer buf i1
  _checkIndexBuffer buf i2
  unsafeSwapBack buf i1 i2

-- | \(O(1)\)
{-# INLINE unsafeSwapFront #-}
unsafeSwapFront :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> Int -> m ()
unsafeSwapFront Buffer {..} i1 i2 = do
  f <- GM.unsafeRead bufferVars _bufferFrontPos
  GM.unsafeSwap internalBuffer (f + i1) (f + i2)

-- | \(O(1)\)
{-# INLINE unsafeSwapBack #-}
unsafeSwapBack :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> Int -> m ()
unsafeSwapBack Buffer {..} i1 i2 = do
  b <- GM.unsafeRead bufferVars _bufferBackPos
  GM.unsafeSwap internalBuffer (b - i1) (b - i2)

-- | \(O(1)\)
{-# INLINE modifyFront #-}
modifyFront :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> (a -> a) -> Int -> m ()
modifyFront buf m i = do
  _checkIndexBuffer buf i
  unsafeModifyFront buf m i

-- | \(O(1)\)
{-# INLINE modifyBack #-}
modifyBack :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> (a -> a) -> Int -> m ()
modifyBack buf m i = do
  _checkIndexBuffer buf i
  unsafeModifyBack buf m i

-- | \(O(1)\)
{-# INLINE unsafeModifyFront #-}
unsafeModifyFront :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> (a -> a) -> Int -> m ()
unsafeModifyFront Buffer {..} m i = do
  f <- GM.unsafeRead bufferVars _bufferFrontPos
  GM.unsafeModify internalBuffer m (f + i)

-- | \(O(1)\)
{-# INLINE unsafeModifyBack #-}
unsafeModifyBack :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> (a -> a) -> Int -> m ()
unsafeModifyBack Buffer {..} m i = do
  b <- GM.unsafeRead bufferVars _bufferBackPos
  GM.unsafeModify internalBuffer m (b - i)

-- | \(O(1)\)
{-# INLINE modifyMFront #-}
modifyMFront :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> (a -> m a) -> Int -> m ()
modifyMFront buf m i = do
  _checkIndexBuffer buf i
  unsafeModifyMFront buf m i

-- | \(O(1)\)
{-# INLINE modifyMBack #-}
modifyMBack :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> (a -> m a) -> Int -> m ()
modifyMBack buf m i = do
  _checkIndexBuffer buf i
  unsafeModifyMBack buf m i

-- | \(O(1)\)
{-# INLINE unsafeModifyMFront #-}
unsafeModifyMFront :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> (a -> m a) -> Int -> m ()
unsafeModifyMFront Buffer {..} m i = do
  f <- GM.unsafeRead bufferVars _bufferFrontPos
  GM.unsafeModifyM internalBuffer m (f + i)

-- | \(O(1)\)
{-# INLINE unsafeModifyMBack #-}
unsafeModifyMBack :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> (a -> m a) -> Int -> m ()
unsafeModifyMBack Buffer {..} m i = do
  b <- GM.unsafeRead bufferVars _bufferBackPos
  GM.unsafeModifyM internalBuffer m (b - i)

-- | \(O(1)\)
{-# INLINE exchangeFront #-}
exchangeFront :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> a -> m a
exchangeFront buf i x = do
  _checkIndexBuffer buf i
  unsafeExchangeFront buf i x

-- | \(O(1)\)
{-# INLINE exchangeBack #-}
exchangeBack :: (HasCallStack, U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> a -> m a
exchangeBack buf i x = do
  _checkIndexBuffer buf i
  unsafeExchangeBack buf i x

-- | \(O(1)\)
{-# INLINE unsafeExchangeFront #-}
unsafeExchangeFront :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> a -> m a
unsafeExchangeFront Buffer {..} i x = do
  f <- GM.unsafeRead bufferVars _bufferFrontPos
  GM.unsafeExchange internalBuffer (f + i) x

-- | \(O(1)\)
{-# INLINE unsafeExchangeBack #-}
unsafeExchangeBack :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> a -> m a
unsafeExchangeBack Buffer {..} i x = do
  b <- GM.unsafeRead bufferVars _bufferBackPos
  GM.unsafeExchange internalBuffer (b - i) x

-- | \(O(N)\)
cloneBuffer :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> m (Buffer (PrimState m) a)
cloneBuffer Buffer {..} = do
  vars' <- GM.clone bufferVars
  buf' <- GM.clone internalBuffer
  pure $ Buffer vars' initialBufferPos buf' internalBufferSize
