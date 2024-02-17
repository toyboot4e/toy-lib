{-# LANGUAGE RecordWildCards #-}

-- | [Data.Buffer](https://github.com/cojna/iota/blob/master/src/Data/Buffer.hs) taken from [cojna/iota](https://github.com/cojna/iota) (thanks!)
module Data.Buffer where

import Control.Applicative
import Control.Exception (assert)
import Control.Monad (void)
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Ix
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

data Buffer s a = Buffer
  { bufferVars :: !(UM.MVector s Int),
    internalBuffer :: !(UM.MVector s a),
    internalBufferSize :: !Int
  }

_bufferFrontPos :: Int
_bufferFrontPos = 0

_bufferBackPos :: Int
_bufferBackPos = 1

newBuffer :: (U.Unbox a, PrimMonad m) => Int -> m (Buffer (PrimState m) a)
newBuffer n = Buffer <$> UM.replicate 2 0 <*> UM.unsafeNew n <*> pure n

type Stack s a = Buffer s a

newBufferAsStack :: (U.Unbox a, PrimMonad m) => Int -> m (Buffer (PrimState m) a)
newBufferAsStack n = Buffer <$> UM.replicate 2 0 <*> UM.unsafeNew n <*> pure n

createBuffer :: (U.Unbox a) => (forall s. ST s (Buffer s a)) -> U.Vector a
createBuffer f = runST $ do
  !buf <- f
  unsafeFreezeBuffer buf

type Queue s a = Buffer s a

newBufferAsQueue :: (U.Unbox a, PrimMonad m) => Int -> m (Buffer (PrimState m) a)
newBufferAsQueue n = Buffer <$> UM.replicate 2 0 <*> UM.unsafeNew n <*> pure n

type Deque s a = Buffer s a

newBufferAsDeque :: (U.Unbox a, PrimMonad m) => Int -> m (Buffer (PrimState m) a)
newBufferAsDeque n =
  Buffer
    <$> UM.replicate 2 n
    <*> UM.unsafeNew (2 * n)
    <*> pure (2 * n)

lengthBuffer :: (PrimMonad m) => Buffer (PrimState m) a -> m Int
lengthBuffer Buffer {bufferVars} =
  liftA2
    (-)
    (UM.unsafeRead bufferVars _bufferBackPos)
    (UM.unsafeRead bufferVars _bufferFrontPos)
{-# INLINE lengthBuffer #-}

nullBuffer :: (PrimMonad m) => Buffer (PrimState m) a -> m Bool
nullBuffer = fmap (== 0) . lengthBuffer
{-# INLINE nullBuffer #-}

clearBuffer :: (PrimMonad m) => Buffer (PrimState m) a -> m ()
clearBuffer Buffer {bufferVars} = do
  UM.unsafeWrite bufferVars _bufferFrontPos 0
  UM.unsafeWrite bufferVars _bufferBackPos 0

freezeBuffer ::
  (U.Unbox a, PrimMonad m) =>
  Buffer (PrimState m) a ->
  m (U.Vector a)
freezeBuffer Buffer {bufferVars, internalBuffer} = do
  f <- UM.unsafeRead bufferVars _bufferFrontPos
  b <- UM.unsafeRead bufferVars _bufferBackPos
  U.freeze $ UM.unsafeSlice f (b - f) internalBuffer

unsafeFreezeBuffer ::
  (U.Unbox a, PrimMonad m) =>
  Buffer (PrimState m) a ->
  m (U.Vector a)
unsafeFreezeBuffer Buffer {bufferVars, internalBuffer} = do
  f <- UM.unsafeRead bufferVars _bufferFrontPos
  b <- UM.unsafeRead bufferVars _bufferBackPos
  U.unsafeFreeze $ UM.unsafeSlice f (b - f) internalBuffer

freezeInternalBuffer ::
  (U.Unbox a, PrimMonad m) =>
  Buffer (PrimState m) a ->
  m (U.Vector a)
freezeInternalBuffer Buffer {bufferVars, internalBuffer} = do
  b <- UM.unsafeRead bufferVars _bufferBackPos
  U.freeze $ UM.unsafeSlice 0 b internalBuffer

unsafeFreezeInternalBuffer ::
  (U.Unbox a, PrimMonad m) =>
  Buffer (PrimState m) a ->
  m (U.Vector a)
unsafeFreezeInternalBuffer Buffer {bufferVars, internalBuffer} = do
  b <- UM.unsafeRead bufferVars _bufferBackPos
  U.unsafeFreeze $ UM.unsafeSlice 0 b internalBuffer

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

viewFront :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> m (Maybe a)
viewFront Buffer {bufferVars, internalBuffer} = do
  f <- UM.unsafeRead bufferVars _bufferFrontPos
  b <- UM.unsafeRead bufferVars _bufferBackPos
  if f < b
    then pure <$> UM.unsafeRead internalBuffer f
    else return Nothing
{-# INLINE viewFront #-}

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

popBack_ :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> m ()
popBack_ = void . popBack
{-# INLINE popBack_ #-}

viewBack :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> m (Maybe a)
viewBack Buffer {bufferVars, internalBuffer} = do
  f <- UM.unsafeRead bufferVars _bufferFrontPos
  b <- UM.unsafeRead bufferVars _bufferBackPos
  if f < b
    then pure <$> UM.unsafeRead internalBuffer (b - 1)
    else return Nothing
{-# INLINE viewBack #-}

pushFront :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> a -> m ()
pushFront Buffer {bufferVars, internalBuffer} x = do
  f <- UM.unsafeRead bufferVars _bufferFrontPos
  UM.unsafeWrite bufferVars _bufferFrontPos (f - 1)
  assert (f > 0) $ do
    UM.unsafeWrite internalBuffer (f - 1) x
{-# INLINE pushFront #-}

pushBack :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> a -> m ()
pushBack Buffer {bufferVars, internalBuffer, internalBufferSize} x = do
  b <- UM.unsafeRead bufferVars _bufferBackPos
  UM.unsafeWrite bufferVars _bufferBackPos (b + 1)
  assert (b < internalBufferSize) $ do
    UM.unsafeWrite internalBuffer b x
{-# INLINE pushBack #-}

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

viewFrontN :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> m (Maybe a)
viewFrontN Buffer {..} i = do
  !f <- UM.unsafeRead bufferVars _bufferFrontPos
  !b <- UM.unsafeRead bufferVars _bufferBackPos
  if inRange (f, b - 1) (f + i)
    then Just <$> UM.read internalBuffer (f + i)
    else return Nothing
{-# INLINE viewFrontN #-}

viewBackN :: (U.Unbox a, PrimMonad m) => Buffer (PrimState m) a -> Int -> m (Maybe a)
viewBackN Buffer {..} i = do
  !f <- UM.unsafeRead bufferVars _bufferFrontPos
  !b <- UM.unsafeRead bufferVars _bufferBackPos
  if inRange (f, b - 1) (b - 1 - i)
    then Just <$> UM.read internalBuffer (b - 1 - i)
    else return Nothing
{-# INLINE viewBackN #-}
