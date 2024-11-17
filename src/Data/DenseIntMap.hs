{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Dense int map or a 64-ary tree that covers @[0, n)@.
--
-- <https://github.com/maspypy/library/blob/main/ds/fastset.hpp>
--
-- FIXME: too slow compared to the original implementation.
module Data.DenseIntMap where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bits
import Data.DenseIntSet
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Stack (HasCallStack)

-- | Dense int map or a 64-ary tree that covers @[0, n)@.
data DenseIntMap s a = DenseIntMap
  { setDIM :: !(DenseIntSet s),
    valDIM :: !(UM.MVector s a)
  }

-- | \(O(N \log N)\) Creates a new `DenseIntMap` that covers @[0, n)@.
{-# INLINE newDIM #-}
newDIM :: (PrimMonad m, U.Unbox a) => Int -> m (DenseIntMap (PrimState m) a)
newDIM cap = do
  setDIM <- newDIS cap
  valDIM <- UM.unsafeNew cap
  return DenseIntMap {..}

-- | \(O(1)\) Returns the number of elements in the map.
{-# INLINE sizeDIM #-}
sizeDIM :: (PrimMonad m) => DenseIntMap (PrimState m) a -> m Int
sizeDIM = sizeDIS . setDIM

-- | \(O(\log N)\) Tests if @k@ is in the map.
{-# INLINE memberDIM #-}
memberDIM :: (PrimMonad m) => DenseIntMap (PrimState m) a -> Int -> m Bool
memberDIM = memberDIS . setDIM

-- | \(O(\log N)\) Tests if @k@ is not in the map.
{-# INLINE notMemberDIM #-}
notMemberDIM :: (PrimMonad m) => DenseIntMap (PrimState m) a -> Int -> m Bool
notMemberDIM = notMemberDIS . setDIM

-- | \(O(\log N)\) Inserts @k@ to the set.
{-# INLINE insertDIM #-}
insertDIM :: (PrimMonad m, U.Unbox a) => DenseIntMap (PrimState m) a -> Int -> a -> m ()
insertDIM DenseIntMap {..} k v = do
  insertDIS setDIM k
  GM.write valDIM k v

-- | \(O(\log N)\) Deletes @k@ from the set.
{-# INLINE deleteDIM #-}
deleteDIM :: (PrimMonad m) => DenseIntMap (PrimState m) a -> Int -> m ()
deleteDIM DenseIntMap {..} k = do
  deleteDIS setDIM k

-- * GT / GE

-- | \(O(\log N)\) Finds the smallest @k'@ s.t. @k' >= k@ in the set.
{-# INLINE lookupGEDIM #-}
lookupGEDIM :: (PrimMonad m, U.Unbox a) => DenseIntMap (PrimState m) a -> Int -> m (Maybe (Int, a))
lookupGEDIM DenseIntMap {..} k = do
  lookupGEDIS setDIM k >>= \case
    Just i -> Just . (i,) <$> GM.read valDIM i
    Nothing -> return Nothing

-- | \(O(\log N)\)
{-# INLINE findGEDIM #-}
findGEDIM :: (PrimMonad m, U.Unbox a) => DenseIntMap (PrimState m) a -> Int -> m (Int, a)
findGEDIM is k = fromMaybe err <$> lookupGEDIM is k
  where
    err = error $ "findGEDIM: no element >= " ++ show k

-- | \(O(\log N)\)
{-# INLINE lookupGTDIM #-}
lookupGTDIM :: (PrimMonad m, U.Unbox a) => DenseIntMap (PrimState m) a -> Int -> m (Maybe (Int, a))
lookupGTDIM is k = lookupGEDIM is (k + 1)

-- | \(O(\log N)\)
{-# INLINE findGTDIM #-}
findGTDIM :: (PrimMonad m, U.Unbox a) => DenseIntMap (PrimState m) a -> Int -> m (Int, a)
findGTDIM is k = findGEDIM is (k + 1)

-- * LT / LE

-- | \(O(\log N)\) Finds the biggest @k'@ s.t. @k' <= k@ in the set.
{-# INLINE lookupLEDIM #-}
lookupLEDIM :: (PrimMonad m, U.Unbox a) => DenseIntMap (PrimState m) a -> Int -> m (Maybe (Int, a))
lookupLEDIM DenseIntMap {..} k = do
  lookupLEDIS setDIM k >>= \case
    Just i -> Just . (i,) <$> GM.read valDIM i
    Nothing -> return Nothing

-- | \(O(\log N)\)
{-# INLINE findLEDIM #-}
findLEDIM :: (PrimMonad m, U.Unbox a) => DenseIntMap (PrimState m) a -> Int -> m (Int, a)
findLEDIM is k = fromMaybe err <$> lookupLEDIM is k
  where
    err = error $ "findLEDIM: no element <= " ++ show k

-- | \(O(\log N)\)
{-# INLINE lookupLTDIM #-}
lookupLTDIM :: (PrimMonad m, U.Unbox a) => DenseIntMap (PrimState m) a -> Int -> m (Maybe (Int, a))
lookupLTDIM is k = lookupLEDIM is (k - 1)

-- | \(O(\log N)\)
{-# INLINE findLTDIM #-}
findLTDIM :: (PrimMonad m, U.Unbox a) => DenseIntMap (PrimState m) a -> Int -> m (Int, a)
findLTDIM is k = findLEDIM is (k - 1)

-- * Min / Max

-- | \(O(\log N)\)
{-# INLINE lookupMinDIM #-}
lookupMinDIM :: (PrimMonad m, U.Unbox a) => DenseIntMap (PrimState m) a -> m (Maybe (Int, a))
lookupMinDIM is = lookupGEDIM is 0

-- | \(O(\log N)\)
{-# INLINE findMinDIM #-}
findMinDIM :: (HasCallStack, PrimMonad m, U.Unbox a) => DenseIntMap (PrimState m) a -> m (Int, a)
findMinDIM is = fromMaybe err <$> lookupMinDIM is
  where
    err = error "findMinDIM: not such a value"

-- | \(O(\log N)\)
{-# INLINE deleteFindMinDIM #-}
deleteFindMinDIM :: (HasCallStack, PrimMonad m, U.Unbox a) => DenseIntMap (PrimState m) a -> m (Int, a)
deleteFindMinDIM is = do
  (!k, !v) <- findMinDIM is
  deleteDIM is k
  return (k, v)

-- | \(O(\log N)\)
{-# INLINE lookupMaxDIM #-}
lookupMaxDIM :: (PrimMonad m, U.Unbox a) => DenseIntMap (PrimState m) a -> m (Maybe (Int, a))
lookupMaxDIM im = lookupLEDIM im (capacityDIS (setDIM im) - 1)

-- | \(O(\log N)\)
{-# INLINE findMaxDIM #-}
findMaxDIM :: (HasCallStack, PrimMonad m, U.Unbox a) => DenseIntMap (PrimState m) a -> m (Int, a)
findMaxDIM im = fromMaybe err <$> lookupMaxDIM im
  where
    err = error "findMaxDIM: not such a value"

-- | \(O(\log N)\)
{-# INLINE deleteFindMaxDIM #-}
deleteFindMaxDIM :: (HasCallStack, PrimMonad m, U.Unbox a) => DenseIntMap (PrimState m) a -> m (Int, a)
deleteFindMaxDIM im = do
  (!k, !v) <- findMaxDIM im
  deleteDIM im k
  return (k, v)

-- | \(O(N)\)
{-# INLINE unsafeKeysDIM #-}
unsafeKeysDIM :: (PrimMonad m, U.Unbox a) => DenseIntMap (PrimState m) a -> m (U.Vector (Int, a))
unsafeKeysDIM im = do
  bits <- U.unsafeFreeze (V.head (vecDIS (setDIM im)))
  vec <- U.unsafeFreeze (valDIM im)
  return
    . U.map
      (\i -> (i, vec G.! i))
    . U.filter
      ( \i ->
          let (!q, !r) = i `divMod` wordDIS
           in testBit (bits G.! q) r
      )
    $ U.generate (wordDIS * U.length vec) id
