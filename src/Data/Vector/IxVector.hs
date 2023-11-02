{-# LANGUAGE RecordWildCards #-}

-- | `Ix`-based API over `vector`.
module Data.Vector.IxVector where

import Control.Monad (forM_)
import Control.Monad.Primitive
import Data.Ix
import Data.Tuple.Extra (first)
import Data.Unindex
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Ix (unsafeIndex)
import ToyLib.Macro (dbgAssert)
import GHC.Stack (HasCallStack)

-- | N-dimensional @Vector@ or @MVector@ with `Data.Ix`.
data IxVector i v = IxVector {boundsIV :: !(i, i), vecIV :: !v}
  deriving (Show, Eq)

-- | Primary `IxVector` type notation.
type IxUVector i a = IxVector i (U.Vector a)

-- | Primary `IxVector` type notation.
type IxMUVector i s a = IxVector i (UM.MVector s a)

-- | Partial `IxVector` accessor
{-# INLINE (@!) #-}
(@!) :: (HasCallStack, Ix i, G.Vector v a) => IxVector i (v a) -> i -> a
(@!) IxVector {..} i = vecIV G.! index boundsIV i

-- | Partial unsafe `IxVector` accessor
{-# INLINE (@!!) #-}
(@!!) :: (Ix i, G.Vector v a) => IxVector i (v a) -> i -> a
(@!!) IxVector {..} i = G.unsafeIndex vecIV (unsafeIndex boundsIV i)

-- | Total `IxVector` accessor
{-# INLINE (@!?) #-}
(@!?) :: (HasCallStack, Ix i, G.Vector v a) => IxVector i (v a) -> i -> Maybe a
(@!?) IxVector {..} i
  -- NOTE: `index` panics on out of range
  | inRange boundsIV i = Just (vecIV G.! index boundsIV i)
  | otherwise = Nothing

-- | Total unsafe `IxVector` accessor
{-# INLINE (@!!?) #-}
(@!!?) :: (Ix i, G.Vector v a) => IxVector i (v a) -> i -> Maybe a
(@!!?) IxVector {..} i
  -- NOTE: `index` panics on out of range
  | inRange boundsIV i = Just (G.unsafeIndex vecIV (unsafeIndex boundsIV i))
  | otherwise = Nothing

mapIV :: (U.Unbox a, U.Unbox b) => (a -> b) -> IxVector i (U.Vector a) -> IxVector i (U.Vector b)
mapIV !f !vec = IxVector bnd $ U.map f (vecIV vec)
  where
    !bnd = boundsIV vec

imapIV :: (Unindex i, U.Unbox a, U.Unbox b) => (i -> a -> b) -> IxVector i (U.Vector a) -> IxVector i (U.Vector b)
imapIV !f !vec = IxVector bnd $ U.imap wrapper (vecIV vec)
  where
    !bnd = boundsIV vec
    wrapper i = f (unindex bnd i)

zipWithIV :: (U.Unbox a, U.Unbox b, U.Unbox c) => (a -> b -> c) -> IxVector i (U.Vector a) -> IxVector i (U.Vector b) -> IxVector i (U.Vector c)
zipWithIV !f !vec1 !vec2 = IxVector bnd $ U.zipWith f (vecIV vec1) (vecIV vec2)
  where
    !bnd = boundsIV vec1

-- | Altertnative to `accumulate` for `IxVector` that share the same bounds.
accumulateIV :: (Ix i, U.Unbox i, U.Unbox a, U.Unbox b) => (a -> b -> a) -> IxVector i (U.Vector a) -> IxVector i (U.Vector (i, b)) -> IxVector i (U.Vector a)
accumulateIV !f !vec0 !commands =
  let !input1d = U.map (first (index bnd)) (vecIV commands)
      !vec1d = U.accumulate f (vecIV vec0) input1d
   in IxVector bnd vec1d
  where
    !bnd = boundsIV vec0
    !_ = dbgAssert (boundsIV vec0 == boundsIV commands)

-- TOOD: `ixmapIV` and rotation examples

-- TODO: `createIx` where we freeze the `IxVector`
-- createIx :: (G.Vector v a, Ix i) => (forall s. ST s (Mutable v s a)) -> IxVector i (v a)
-- createIx = runST

-- replicateIVM

-- | Reads a value from `IxVector`.
{-# INLINE readIV #-}
readIV :: (HasCallStack, Ix i, PrimMonad m, GM.MVector v a) => IxVector i (v (PrimState m) a) -> i -> m a
readIV IxVector {..} i = GM.read vecIV (index boundsIV i)

{-# INLINE unsafeReadIV #-}
unsafeReadIV :: (Ix i, PrimMonad m, GM.MVector v a) => IxVector i (v (PrimState m) a) -> i -> m a
unsafeReadIV IxVector {..} i = GM.unsafeRead vecIV (unsafeIndex boundsIV i)

-- | Writes a value to `IxVector`.
{-# INLINE writeIV #-}
writeIV :: (HasCallStack, Ix i, PrimMonad m, GM.MVector v a) => IxVector i (v (PrimState m) a) -> i -> a -> m ()
writeIV IxVector {..} i = GM.write vecIV (index boundsIV i)

{-# INLINE unsafeWriteIV #-}
unsafeWriteIV :: (Ix i, PrimMonad m, GM.MVector v a) => IxVector i (v (PrimState m) a) -> i -> a -> m ()
unsafeWriteIV IxVector {..} i = GM.unsafeWrite vecIV (unsafeIndex boundsIV i)

{-# INLINE modifyIV #-}
modifyIV :: (HasCallStack, Ix i, PrimMonad m, GM.MVector v a) => IxVector i (v (PrimState m) a) -> (a -> a) -> i -> m ()
modifyIV IxVector {..} !alter i = GM.modify vecIV alter (index boundsIV i)

{-# INLINE unsafeModifyIV #-}
unsafeModifyIV :: (Ix i, PrimMonad m, GM.MVector v a) => IxVector i (v (PrimState m) a) -> (a -> a) -> i -> m ()
unsafeModifyIV IxVector {..} !alter i = GM.unsafeModify vecIV alter (unsafeIndex boundsIV i)

{-# INLINE modifyMIV #-}
modifyMIV :: (HasCallStack, Ix i, PrimMonad m, GM.MVector v a) => IxVector i (v (PrimState m) a) -> (a -> m a) -> i -> m ()
modifyMIV IxVector {..} !alter i = GM.modifyM vecIV alter (index boundsIV i)

{-# INLINE unsafeModifyMIV #-}
unsafeModifyMIV :: (Ix i, PrimMonad m, GM.MVector v a) => IxVector i (v (PrimState m) a) -> (a -> m a) -> i -> m ()
unsafeModifyMIV IxVector {..} !alter i = GM.unsafeModifyM vecIV alter (unsafeIndex boundsIV i)

{-# INLINE swapIV #-}
swapIV :: (HasCallStack, Ix i, PrimMonad m, GM.MVector v a) => IxVector i (v (PrimState m) a) -> i -> i -> m ()
swapIV IxVector {..} !i1 !i2 = GM.swap vecIV (index boundsIV i1) (index boundsIV i2)

{-# INLINE unsafeSwapIV #-}
unsafeSwapIV :: (Ix i, PrimMonad m, GM.MVector v a) => IxVector i (v (PrimState m) a) -> i -> i -> m ()
unsafeSwapIV IxVector {..} !i1 !i2 = GM.unsafeSwap vecIV (unsafeIndex boundsIV i1) (unsafeIndex boundsIV i2)

-- | WARNING: Can you really allocate/run \(O(HW)\) algorithm?
imos2DIV :: HasCallStack => IxVector (Int, Int) (U.Vector Int) -> IxVector (Int, Int) (U.Vector Int)
imos2DIV seeds@IxVector {boundsIV} = IxVector boundsIV $ U.create $ do
  !vec <- IxVector boundsIV <$> U.thaw (vecIV seeds)

  let (!minY, !minX) = fst boundsIV

  -- row scan
  forM_ (range boundsIV) $ \(!y, !x) -> do
    !v <- if x == minX then return 0 else readIV vec (y, x - 1)
    modifyIV vec (+ v) (y, x)

  -- column scan
  forM_ (range boundsIV) $ \(!x, !y) -> do
    !v <- if y == minY then return 0 else readIV vec (y - 1, x)
    modifyIV vec (+ v) (y, x)

  return $ vecIV vec
