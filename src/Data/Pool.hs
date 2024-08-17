{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Fixed-sized array for \(O(1)\) allocation and \(O(1)\) clearing after \(O(N)\) construction.
module Data.Pool where

import Control.Exception (assert)
import Control.Monad (unless, void, when)
import Control.Monad.Fix (fix)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Buffer
import Data.Maybe
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Stack (HasCallStack)

-- | Fixed-sized array for \(O(1)\) allocation.
data Pool s a = Pool
  { -- | Data array.
    dataPool :: !(UM.MVector s a),
    -- | Free slot indices after deallocation.
    freePool :: !(Buffer s Int),
    -- | Next index when `freePool` is empty.
    nextPool :: !(UM.MVector s Int)
  }

-- | Index of an element in the `Pool`.
type PoolIndex = Int

-- | Invalid, @null@ `PoolIndex`.
{-# INLINE undefPI #-}
undefPI :: PoolIndex
undefPI = -1

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
  nextPool <- UM.replicate 1 (0 :: Int)
  return Pool {..}

-- | \(O(1)\) Returns the maximum number of elements the pool can store.
{-# INLINE capacityPool #-}
capacityPool :: (U.Unbox a) => Pool s a -> Int
capacityPool = GM.length . dataPool

-- | \(O(1)\) Returns the number of elements in the pool.
{-# INLINE sizePool #-}
sizePool :: (PrimMonad m,U.Unbox a) => Pool (PrimState m) a -> m Int
sizePool Pool {..} = do
  !nFree <- lengthBuffer freePool
  !next <- GM.read nextPool 0
  let !cap = GM.length dataPool
  return $ cap - (next - nFree)

-- | \(O(1)\) Resets the pool to the initial state.
{-# INLINE clearPool #-}
clearPool :: (PrimMonad m, U.Unbox a) => Pool (PrimState m) a -> m()
clearPool Pool {..} = do
  clearBuffer freePool
  GM.write nextPool 0 0

-- | \(O(1)\) Allocates a new element. TODO: capacity validation?
{-# INLINE allocPool #-}
allocPool :: (PrimMonad m, U.Unbox a) => Pool (PrimState m) a -> a -> m PoolIndex
allocPool Pool {..} !x = do
  popFront freePool >>= \case
    Just i -> return i
    Nothing -> do
      i <- GM.read nextPool 0
      GM.write nextPool 0 (i + 1)
      GM.write dataPool i x
      return i
  
-- | \(O(1)\) Deallocates an element. Be sure to not deallocate a deleted element.
-- TODO: consider setting up validation of slots?
{-# INLINE deallocPool #-}
deallocPool :: (PrimMonad m, U.Unbox a) => Pool (PrimState m) a -> PoolIndex -> m ()
deallocPool Pool {..} i= do
  pushFront freePool i

