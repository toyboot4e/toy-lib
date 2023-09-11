-- | [Data.Buffer](https://github.com/cojna/iota/blob/master/src/Data/Buffer.hs) taken from [cojna/iota](https://github.com/cojna/iota) (thanks!)
module Data.Buffer where

import Control.Applicative
import Control.Exception (assert)
import Control.Monad.Primitive
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

data Buffer s a = Buffer
  { bufferVars :: !(VUM.MVector s Int),
    internalBuffer :: !(VUM.MVector s a),
    internalBufferSize :: !Int
  }

_bufferFrontPos :: Int
_bufferFrontPos = 0

_bufferBackPos :: Int
_bufferBackPos = 1

newBuffer :: (VU.Unbox a, PrimMonad m) => Int -> m (Buffer (PrimState m) a)
newBuffer n = Buffer <$> VUM.replicate 2 0 <*> VUM.unsafeNew n <*> pure n

type Stack s a = Buffer s a

newBufferAsStack :: (VU.Unbox a, PrimMonad m) => Int -> m (Buffer (PrimState m) a)
newBufferAsStack n = Buffer <$> VUM.replicate 2 0 <*> VUM.unsafeNew n <*> pure n

type Queue s a = Buffer s a

newBufferAsQueue :: (VU.Unbox a, PrimMonad m) => Int -> m (Buffer (PrimState m) a)
newBufferAsQueue n = Buffer <$> VUM.replicate 2 0 <*> VUM.unsafeNew n <*> pure n

type Deque s a = Buffer s a

newBufferAsDeque :: (VU.Unbox a, PrimMonad m) => Int -> m (Buffer (PrimState m) a)
newBufferAsDeque n =
  Buffer
    <$> VUM.replicate 2 n
    <*> VUM.unsafeNew (2 * n)
    <*> pure (2 * n)

lengthBuffer :: (PrimMonad m) => Buffer (PrimState m) a -> m Int
lengthBuffer Buffer {bufferVars} =
  liftA2
    (-)
    (VUM.unsafeRead bufferVars _bufferBackPos)
    (VUM.unsafeRead bufferVars _bufferFrontPos)
{-# INLINE lengthBuffer #-}

nullBuffer :: (PrimMonad m) => Buffer (PrimState m) a -> m Bool
nullBuffer = fmap (== 0) . lengthBuffer
{-# INLINE nullBuffer #-}

clearBuffer :: (PrimMonad m) => Buffer (PrimState m) a -> m ()
clearBuffer Buffer {bufferVars} = do
  VUM.unsafeWrite bufferVars _bufferFrontPos 0
  VUM.unsafeWrite bufferVars _bufferBackPos 0

freezeBuffer ::
  (VU.Unbox a, PrimMonad m) =>
  Buffer (PrimState m) a ->
  m (VU.Vector a)
freezeBuffer Buffer {bufferVars, internalBuffer} = do
  f <- VUM.unsafeRead bufferVars _bufferFrontPos
  b <- VUM.unsafeRead bufferVars _bufferBackPos
  VU.freeze $ VUM.unsafeSlice f (b - f) internalBuffer

unsafeFreezeBuffer ::
  (VU.Unbox a, PrimMonad m) =>
  Buffer (PrimState m) a ->
  m (VU.Vector a)
unsafeFreezeBuffer Buffer {bufferVars, internalBuffer} = do
  f <- VUM.unsafeRead bufferVars _bufferFrontPos
  b <- VUM.unsafeRead bufferVars _bufferBackPos
  VU.unsafeFreeze $ VUM.unsafeSlice f (b - f) internalBuffer

freezeInternalBuffer ::
  (VU.Unbox a, PrimMonad m) =>
  Buffer (PrimState m) a ->
  m (VU.Vector a)
freezeInternalBuffer Buffer {bufferVars, internalBuffer} = do
  b <- VUM.unsafeRead bufferVars _bufferBackPos
  VU.freeze $ VUM.unsafeSlice 0 b internalBuffer

unsafeFreezeInternalBuffer ::
  (VU.Unbox a, PrimMonad m) =>
  Buffer (PrimState m) a ->
  m (VU.Vector a)
unsafeFreezeInternalBuffer Buffer {bufferVars, internalBuffer} = do
  b <- VUM.unsafeRead bufferVars _bufferBackPos
  VU.unsafeFreeze $ VUM.unsafeSlice 0 b internalBuffer

popFront :: (VU.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> m (Maybe a)
popFront Buffer {bufferVars, internalBuffer} = do
  f <- VUM.unsafeRead bufferVars _bufferFrontPos
  b <- VUM.unsafeRead bufferVars _bufferBackPos
  if f < b
    then do
      VUM.unsafeWrite bufferVars _bufferFrontPos (f + 1)
      pure <$> VUM.unsafeRead internalBuffer f
    else return Nothing
{-# INLINE popFront #-}

viewFront :: (VU.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> m (Maybe a)
viewFront Buffer {bufferVars, internalBuffer} = do
  f <- VUM.unsafeRead bufferVars _bufferFrontPos
  b <- VUM.unsafeRead bufferVars _bufferBackPos
  if f < b
    then pure <$> VUM.unsafeRead internalBuffer f
    else return Nothing
{-# INLINE viewFront #-}

popBack :: (VU.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> m (Maybe a)
popBack Buffer {bufferVars, internalBuffer} = do
  f <- VUM.unsafeRead bufferVars _bufferFrontPos
  b <- VUM.unsafeRead bufferVars _bufferBackPos
  if f < b
    then do
      VUM.unsafeWrite bufferVars _bufferBackPos (b - 1)
      pure <$> VUM.unsafeRead internalBuffer (b - 1)
    else return Nothing
{-# INLINE popBack #-}

viewBack :: (VU.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> m (Maybe a)
viewBack Buffer {bufferVars, internalBuffer} = do
  f <- VUM.unsafeRead bufferVars _bufferFrontPos
  b <- VUM.unsafeRead bufferVars _bufferBackPos
  if f < b
    then pure <$> VUM.unsafeRead internalBuffer (b - 1)
    else return Nothing
{-# INLINE viewBack #-}

pushFront :: (VU.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> a -> m ()
pushFront Buffer {bufferVars, internalBuffer} x = do
  f <- VUM.unsafeRead bufferVars _bufferFrontPos
  VUM.unsafeWrite bufferVars _bufferFrontPos (f - 1)
  assert (f > 0) $ do
    VUM.unsafeWrite internalBuffer (f - 1) x
{-# INLINE pushFront #-}

pushBack :: (VU.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> a -> m ()
pushBack Buffer {bufferVars, internalBuffer, internalBufferSize} x = do
  b <- VUM.unsafeRead bufferVars _bufferBackPos
  VUM.unsafeWrite bufferVars _bufferBackPos (b + 1)
  assert (b < internalBufferSize) $ do
    VUM.unsafeWrite internalBuffer b x
{-# INLINE pushBack #-}

pushFronts ::
  (VU.Unbox a, PrimMonad m) =>
  Buffer (PrimState m) a ->
  VU.Vector a ->
  m ()
pushFronts Buffer {bufferVars, internalBuffer} vec = do
  let n = VU.length vec
  f <- VUM.unsafeRead bufferVars _bufferFrontPos
  VUM.unsafeWrite bufferVars _bufferFrontPos (f - n)
  assert (n <= f) $ do
    VU.unsafeCopy (VUM.unsafeSlice (f - n) n internalBuffer) vec
{-# INLINE pushFronts #-}

pushBacks ::
  (VU.Unbox a, PrimMonad m) =>
  Buffer (PrimState m) a ->
  VU.Vector a ->
  m ()
pushBacks Buffer {bufferVars, internalBuffer, internalBufferSize} vec = do
  let n = VU.length vec
  b <- VUM.unsafeRead bufferVars _bufferBackPos
  VUM.unsafeWrite bufferVars _bufferBackPos (b + n)
  assert (b + n - 1 < internalBufferSize) $ do
    VU.unsafeCopy (VUM.unsafeSlice b n internalBuffer) vec
{-# INLINE pushBacks #-}
