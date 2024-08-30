{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | Fixed-sized array for \(O(1)\) allocation and \(O(1)\) clearing after \(O(N)\) construction.
module Data.Pool where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Buffer
import Data.Coerce
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- | Fixed-sized array for \(O(1)\) allocation.
data Pool s a = Pool
  { -- | Data array.
    dataPool :: !(UM.MVector s a),
    -- | Free slot indices after deallocation.
    freePool :: !(Buffer s PoolIndex),
    -- | Next index when `freePool` is empty.
    nextPool :: !(UM.MVector s PoolIndex)
  }

-- | Strongly typed index of pool items. User has to explicitly @corece@ on raw index use, but it's
-- ok as far as the end user don't see it.
newtype PoolIndex = PoolIndex {unPoolIndex :: Int}
  deriving (Eq, P.Prim)
  deriving newtype (Ord, Show)

newtype instance U.MVector s PoolIndex = MV_PoolIndex (P.MVector s PoolIndex)

newtype instance U.Vector PoolIndex = V_PoolIndex (P.Vector PoolIndex)

deriving via (U.UnboxViaPrim PoolIndex) instance GM.MVector UM.MVector PoolIndex

deriving via (U.UnboxViaPrim PoolIndex) instance G.Vector U.Vector PoolIndex

instance U.Unbox PoolIndex

-- | Invalid, @null@ `PoolIndex`.
{-# INLINE undefPI #-}
undefPI :: PoolIndex
undefPI = PoolIndex (-1)

-- | Returns `True` if the index is invalid.
{-# INLINE nullPI #-}
nullPI :: PoolIndex -> Bool
nullPI = (== undefPI)

-- | \(O(N)\) Creates a pool with the specified @capacity@.
{-# INLINE newPool #-}
newPool :: (U.Unbox a, PrimMonad m) => Int -> m (Pool (PrimState m) a)
newPool capacity = do
  dataPool <- UM.unsafeNew capacity
  freePool <- newRevBuffer capacity
  nextPool <- UM.replicate 1 (PoolIndex 0)
  return Pool {..}

-- | \(O(1)\) Returns the maximum number of elements the pool can store.
{-# INLINE capacityPool #-}
capacityPool :: (U.Unbox a) => Pool s a -> Int
capacityPool = GM.length . dataPool

-- | \(O(1)\) Returns the number of elements in the pool.
{-# INLINE sizePool #-}
sizePool :: (PrimMonad m, U.Unbox a) => Pool (PrimState m) a -> m Int
sizePool Pool {..} = do
  !nFree <- lengthBuffer freePool
  PoolIndex !next <- GM.unsafeRead nextPool 0
  let !cap = GM.length dataPool
  return $ cap - (next - nFree)

-- | \(O(1)\) Resets the pool to the initial state.
{-# INLINE clearPool #-}
clearPool :: (PrimMonad m) => Pool (PrimState m) a -> m ()
clearPool Pool {..} = do
  clearBuffer freePool
  GM.unsafeWrite nextPool 0 $ PoolIndex 0

-- | \(O(1)\) Allocates a new element. TODO: capacity validation?
{-# INLINE allocPool #-}
allocPool :: (PrimMonad m, U.Unbox a) => Pool (PrimState m) a -> a -> m PoolIndex
allocPool Pool {..} !x = do
  popFront freePool >>= \case
    Just i -> return i
    Nothing -> do
      PoolIndex i <- GM.unsafeRead nextPool 0
      GM.unsafeWrite nextPool 0 $ coerce (i + 1)
      GM.write dataPool i x
      return $ coerce i

-- | \(O(1)\) Deallocates an element. Be sure to not deallocate a deleted element.
-- TODO: consider setting up validation of slots?
{-# INLINE deallocPool #-}
deallocPool :: (PrimMonad m) => Pool (PrimState m) a -> PoolIndex -> m ()
deallocPool Pool {..} i = do
  pushFront freePool i

-- | \(O(1)\)
{-# INLINE writePool #-}
writePool :: (PrimMonad m, U.Unbox a) => Pool (PrimState m) a -> PoolIndex -> a -> m ()
writePool Pool {dataPool} !i !x = do
  GM.write dataPool (coerce i) x

-- | \(O(1)\)
{-# INLINE modifyPool #-}
modifyPool :: (PrimMonad m, U.Unbox a) => Pool (PrimState m) a -> (a -> a) -> PoolIndex -> m ()
modifyPool Pool {dataPool} !f !i = do
  GM.modify dataPool f (coerce i)

-- | \(O(1)\)
{-# INLINE exchangePool #-}
exchangePool :: (PrimMonad m, U.Unbox a) => Pool (PrimState m) a -> PoolIndex -> a -> m a
exchangePool Pool {dataPool} !i !x = do
  GM.exchange dataPool (coerce i) x

-- | \(O(1)\)
{-# INLINE readPool #-}
readPool :: (PrimMonad m, U.Unbox a) => Pool (PrimState m) a -> PoolIndex -> m a
readPool Pool {dataPool} !i = do
  GM.read dataPool (coerce i)
