{-# LANGUAGE RecordWildCards #-}

-- | Dense integer hash map.
--
-- <https://github.com/maspypy/library/blob/main/ds/hashmap.hpp>
module Data.DenseHashMap where

import Control.Monad
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bits
import Data.Maybe
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Stack (HasCallStack)

-- TODO: no way to invalidate entry?
-- TODO: track the number of entries

-- | Dense int map that holds up up @n@ values.
data DenseHashMap s a = DenseHashMap
  { -- | Maximum number of elements.
    maxCapHM :: !Int,
    -- | The number of elements that can be added.
    restCapHM :: !(UM.MVector s Int),
    -- | Bit mask for powerset iteration on indexing.
    maskHM :: !Int,
    -- | Original key to the hash index.
    keyHM :: !(UM.MVector s Int),
    -- | Values to the hash index.
    valHM :: !(UM.MVector s a),
    -- | If the slot is used or not.
    usedHM :: !(UM.MVector s Bool)
  }

-- | \(O(N)\) Creates a hashmap.
newHM :: (U.Unbox a, PrimMonad m) => Int -> m (DenseHashMap (PrimState m) a)
newHM n = do
  let !k0 = 8
  let !k = until (>= 2 * n) (* 2) k0
  let !maxCapHM = k `div` 2
  restCapHM <- UM.replicate 1 maxCapHM
  let !maskHM = k - 1
  keyHM <- UM.unsafeNew k
  valHM <- UM.unsafeNew k
  usedHM <- UM.replicate k False
  return DenseHashMap {..}

-- | \(O(1)\) Returns the number of stored elements. Not tested
sizeHM :: (PrimMonad m) => DenseHashMap (PrimState m) a -> m Int
sizeHM DenseHashMap{..}= do
  !rest <- UM.unsafeRead restCapHM 0
  return $ maxCapHM - rest

-- | \(O(1)\) Clears the buffer. Not tested
clearHM :: (PrimMonad m) => DenseHashMap (PrimState m) a -> m ()
clearHM DenseHashMap {..} = do
  GM.set usedHM False
  UM.unsafeWrite restCapHM 0 maxCapHM

-- | \(O(1)\) (Internal) Hash value calculation.
hashHM :: DenseHashMap a s -> Int -> Int
hashHM hm x = (x3 `xor` (x3 .>>. 31)) .&. maskHM hm
  where
    fixedRandom = 321896566547
    -- TODO: what is it?
    x1 = x + fixedRandom
    -- TODO: literal out of range. works for int?
    x2 = (x1 `xor` (x1 .>>. 30)) * 0xbf58476d1ce4e5b9
    x3 = (x2 `xor` (x2 .>>. 27)) * 0x94d049bb133111eb

-- | \(O(1)\) (Internal) Hashed slot search.
-- FIXME: capacity check
indexHM :: (PrimMonad m) => DenseHashMap (PrimState m) a -> Int -> m Int
indexHM hm@DenseHashMap {..} k = inner (hashHM hm k)
  where
    inner !h = do
      b <- GM.read usedHM h
      -- already there?
      k' <- GM.read keyHM h
      if b && k' /= k
        then -- TODO: powerset enumeration technique?
          inner $ (h + 1) .&. maskHM
        else return h

memberHM :: (HasCallStack, PrimMonad m) => DenseHashMap (PrimState m) a -> Int -> m Bool
memberHM hm@DenseHashMap {..} k = do
  i <- indexHM hm k
  b <- GM.read usedHM i
  k' <- GM.read keyHM i
  return $ b && k' == k

readHM :: (HasCallStack, U.Unbox a, PrimMonad m) => DenseHashMap (PrimState m) a -> Int -> m a
readHM hm k = fromMaybe err <$> readMayHM hm k
  where
    err = error $ "readHM: cannot find value for key: " ++ show k

readMayHM :: (HasCallStack, U.Unbox a, PrimMonad m) => DenseHashMap (PrimState m) a -> Int -> m (Maybe a)
readMayHM hm@DenseHashMap {..} k = do
  i <- indexHM hm k
  b <- GM.read usedHM i
  if b
    then Just <$> GM.read valHM i
    else return Nothing

-- FIXME: capacity check
writeHM :: (HasCallStack, U.Unbox a, PrimMonad m) => DenseHashMap (PrimState m) a -> Int -> a -> m ()
writeHM hm k v = void $ exchangeHM hm k v

-- FIXME: capacity check
exchangeHM :: (HasCallStack, U.Unbox a, PrimMonad m) => DenseHashMap (PrimState m) a -> Int -> a -> m (Maybe a)
exchangeHM hm@DenseHashMap {..} k v = do
  i <- indexHM hm k
  b <- GM.read usedHM i
  if b
    then do
      -- already stored
      GM.write valHM i v
      return Nothing
    else do
      -- newly inserted value
      GM.unsafeModify restCapHM (subtract 1) 0
      GM.write usedHM i True
      GM.write keyHM i k
      Just <$> GM.exchange valHM i v

-- | Not tested
modifyHM :: (HasCallStack, U.Unbox a, PrimMonad m) => DenseHashMap (PrimState m) a -> (a -> a) -> Int -> m ()
modifyHM hm@DenseHashMap {..} f k = do
  i <- indexHM hm k
  b <- GM.read usedHM i
  if b
    then GM.modify valHM f i
    else error $ "modifyHM: not a member " ++ show k

-- | \(O(1)\) Deletes a value. Not tested
deleteHM :: (HasCallStack, U.Unbox a, PrimMonad m) => DenseHashMap (PrimState m) a -> Int -> m (Maybe a)
deleteHM hm@DenseHashMap {..} k = do
  i <- indexHM hm k
  b <- GM.read usedHM i
  if b
    then do
      -- stored
      GM.unsafeModify restCapHM (+ 1) 0
      GM.write usedHM i False
      Just <$> GM.read valHM i
    else do
      return Nothing

-- | \(O(1)\) Deletes a value. Not tested
deleteHM_ :: (HasCallStack, U.Unbox a, PrimMonad m) => DenseHashMap (PrimState m) a -> Int -> m ()
deleteHM_ hm k = void $ deleteHM hm k

-- | \(O(N)\) Enumerates the stored key-value pairs.
unsafeAssocsHM :: (HasCallStack, U.Unbox a, PrimMonad m) => DenseHashMap (PrimState m) a -> m (U.Vector (Int, a))
unsafeAssocsHM DenseHashMap {..} = do
  key <- U.unsafeFreeze keyHM
  val <- U.unsafeFreeze valHM
  U.filterM (\(!k, !_) -> GM.read usedHM k) $ U.zip key val
