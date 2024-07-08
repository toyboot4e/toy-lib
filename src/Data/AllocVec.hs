{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | Fixed-sized vector for bulk allocation and \(O(1)\) free slot search.
module Data.AllocVec where

import Control.Exception (assert)
import Control.Monad (forM_)
import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Stack (HasCallStack)

-- TODO: assertion with `HasCallStack` should make sense, not redundant?

-- | Index of a data stored in a `AllocVec`.
type IndexAV = Int

-- | Fixed-sized vector for bulk allocation and \(O(1)\) finding free slots.
data AllocVec s a = AllocVec
  { -- | The maximum number of elements `AllocVec` can store.
    capacityAV :: !Int,
    -- | The number of elements stored in `AllocVec`.
    sizeAV :: !Int,
    -- | The internal vector.
    dataAV :: !(UM.MVector s a),
    -- | First empty slot.
    firstFreeAV :: !(UM.MVector s IndexAV),
    -- | Linked list of free slots.
    nextFreeAV :: !(UM.MVector s IndexAV)
  }

-- * New / reset

-- | \(O(1)\) Creates an empty `AllocVec` of length @n@.
newAllocVec :: (U.Unbox a, PrimMonad m) => Int -> m (AllocVec (PrimState m) a)
newAllocVec n = do
  dataAV <- UM.unsafeNew n
  firstFreeAV <- UM.replicate 1 $ if n == 0 then endSlotAV else 0
  nextFreeAV <- UM.generate n $ \i ->
    if i == n - 1
      then endSlotAV
      else i + 1
  return AllocVec {capacityAV = n, sizeAV = 0, ..}

-- | \(O(N)\) Resets `AllocVec` to the initial state.
resetAllocVec :: (PrimMonad m) => AllocVec (PrimState m) a -> m ()
resetAllocVec AllocVec {..}
  | capacityAV == 0 = do
      GM.unsafeWrite firstFreeAV 0 endSlotAV
  | otherwise = do
      forM_ [0 .. capacityAV - 2] $ \i -> do
        GM.unsafeWrite nextFreeAV i (i + 1)
      GM.unsafeWrite nextFreeAV (capacityAV - 1) endSlotAV
      GM.unsafeWrite firstFreeAV 0 (0 :: SlotAV)

-- * Slots

-- | Index to the internal vector in `AllocVec`.
type SlotAV = Int

-- | The end of the free slots.
endSlotAV :: SlotAV
endSlotAV = -2

-- | The slot is not free.
filledSlotAV :: SlotAV
filledSlotAV = -3

-- | Asserts the slot is non-empty.
validateSlotAV :: (HasCallStack) => AllocVec s a -> SlotAV -> ()
validateSlotAV AllocVec {..} i =
  let !_ = assert (i /= endSlotAV) "end of free slots"
      !_ = assert (i /= filledSlotAV) "filled slot"
      !_ = assert (i >= 0 && i < capacityAV) $ "slot out of range: " ++ show (0 :: Int, capacityAV - 1) ++ " " ++ show i
   in ()

-- | Asserts the slot is non-empty.
validateIndexAV :: (HasCallStack) => AllocVec s a -> IndexAV -> ()
validateIndexAV AllocVec {..} i =
  let !_ = assert (i >= 0 && i < capacityAV) $ "index out of range: " ++ show (0 :: Int, capacityAV - 1) ++ " " ++ show i
   in ()

-- * API

-- | \(O(1)\) Allocates a new entry. Panics on saturation.
--
-- = Visualization
--
-- Initial state:
--
-- @
--  *  first free
--  |           +-------+
--  |           |       |
--  v           v       |
-- [ ] [-] [-] [ ] [-] [ ]
--  |                   |
--  +-------------------+
-- @
--
-- After the allocation:
--
-- @
--  * first free
--  |
--  +-------------------+
--                      |
-- [-] [-] [-] [ ] [-] [ ]
--              ^       |
--              |       |
--              +-------+
-- @
allocAV :: (HasCallStack, U.Unbox a, PrimMonad m) => AllocVec (PrimState m) a -> a -> m IndexAV
allocAV av@AllocVec {..} x
  | sizeAV == capacityAV = error "no empty slot"
  | otherwise = do
      (slot :: SlotAV) <- GM.read firstFreeAV 0
      let !_ = validateSlotAV av slot
      GM.write dataAV slot x
      next <- GM.exchange nextFreeAV slot filledSlotAV
      GM.write firstFreeAV 0 next
      return slot

-- | \(O(1)\) Deallocates an existing entry.
--
-- = Visualization
--
-- Initial state:
--
-- @
--  *  first free
--  |           +-------+
--  |           |       |
--  v           v       |
-- [ ] [-] [-] [ ] [-] [ ]
--  |                   |
--  +-------------------+
-- @
--
-- After the deallocation of the second entry:
--
-- @
--        *  first free
--        |
--  +--+  |       +-------+
--  |  |  |       |       |
--  v  |  v       v       |
-- [ ] | [ ] [-] [ ] [-] [ ]
--  |  |  |               |
--  |  +--+               |
--  +---------------------+
-- @
deallocAV :: (HasCallStack, PrimMonad m) => AllocVec (PrimState m) a -> IndexAV -> m ()
deallocAV av@AllocVec {..} i = do
  let !_ = validateIndexAV av i
  oldFirstFree <- GM.exchange firstFreeAV 0 i
  GM.write firstFreeAV 0 i
  GM.write nextFreeAV i oldFirstFree

-- | \(O(1)\) Reads an existing entry.
readAV :: (HasCallStack, U.Unbox a, PrimMonad m) => AllocVec (PrimState m) a -> IndexAV -> m a
readAV av@AllocVec {..} i = do
  let !_ = validateIndexAV av i
  GM.read dataAV i

-- | \(O(1)\) Reads an existing or non-existing entry.
readMaybeAV :: (U.Unbox a, PrimMonad m) => AllocVec (PrimState m) a -> IndexAV -> m (Maybe a)
readMaybeAV AllocVec {..} = GM.readMaybe dataAV

-- | \(O(1)\) Writes to an existing entry.
writeAV :: (HasCallStack, U.Unbox a, PrimMonad m) => AllocVec (PrimState m) a -> IndexAV -> a -> m ()
writeAV av@AllocVec {..} i x = do
  let !_ = validateIndexAV av i
  GM.write dataAV i x

-- | \(O(1)\) Writes to an existing entry.
modifyAV :: (HasCallStack, U.Unbox a, PrimMonad m) => AllocVec (PrimState m) a -> (a -> a) -> IndexAV -> m ()
modifyAV av@AllocVec {..} f i = do
  let !_ = validateIndexAV av i
  GM.modify dataAV f i

-- | \(O(1)\) Writes to an existing entry.
modifyMAV :: (HasCallStack, U.Unbox a, PrimMonad m) => AllocVec (PrimState m) a -> (a -> m a) -> IndexAV -> m ()
modifyMAV av@AllocVec {..} f i = do
  let !_ = validateIndexAV av i
  GM.modifyM dataAV f i

-- | \(O(1)\) Swaps two entries
swapAV :: (HasCallStack, U.Unbox a, PrimMonad m) => AllocVec (PrimState m) a -> IndexAV -> IndexAV -> m ()
swapAV av@AllocVec {..} i1 i2 = do
  let !_ = validateIndexAV av i1
  let !_ = validateIndexAV av i2
  GM.swap dataAV i1 i2

-- | \(O(1)\) Exchanges an existing entry.
exchangeAV :: (HasCallStack, U.Unbox a, PrimMonad m) => AllocVec (PrimState m) a -> IndexAV -> a -> m a
exchangeAV av@AllocVec {..} i x = do
  let !_ = validateIndexAV av i
  GM.exchange dataAV i x
