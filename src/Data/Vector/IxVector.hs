{-# LANGUAGE RecordWildCards #-}

-- | `Ix`-based API over `vector`.
module Data.Vector.IxVector where

import Control.Monad (forM_)
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bifunctor (first, second)
import Data.Ix
import Data.Tuple.Extra (both)
import Data.Utils.Unindex
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Ix (unsafeIndex)
import GHC.Stack (HasCallStack)
import ToyLib.Debug

-- | N-dimensional @Vector@ or @MVector@ with `Data.Ix`.
data IxVector i v = IxVector {boundsIV :: !(i, i), vecIV :: !v}
  deriving (Show, Eq)

-- | Primary `IxVector` type notation.
type IxUVector i a = IxVector i (U.Vector a)

-- | Primary `IxVector` type notation.
type IxMUVector s i a = IxVector i (UM.MVector s a)

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

{-# INLINE lengthIV #-}
lengthIV :: (G.Vector v a) => IxVector i (v a) -> Int
lengthIV = G.length . vecIV

{-# INLINE findIndexIV #-}
findIndexIV :: (G.Vector v a, Unindex i) => IxVector i (v a) -> (a -> Bool) -> Maybe i
findIndexIV IxVector { .. } f = unindex boundsIV <$> G.findIndex f vecIV

{-# INLINE mapIV #-}
mapIV :: (U.Unbox a, U.Unbox b) => (a -> b) -> IxVector i (U.Vector a) -> IxVector i (U.Vector b)
mapIV !f IxVector { .. } = IxVector boundsIV $ U.map f vecIV

{-# INLINE imapIV #-}
imapIV :: (Unindex i, U.Unbox a, U.Unbox b) => (i -> a -> b) -> IxVector i (U.Vector a) -> IxVector i (U.Vector b)
imapIV !f IxVector{..} = IxVector boundsIV $ U.imap (f . unindex boundsIV) vecIV

{-# INLINE zipWithIV #-}
zipWithIV :: (U.Unbox a, U.Unbox b, U.Unbox c) => (a -> b -> c) -> IxVector i (U.Vector a) -> IxVector i (U.Vector b) -> IxVector i (U.Vector c)
zipWithIV !f !vec1 !vec2 = IxVector bnd $ U.zipWith f (vecIV vec1) (vecIV vec2)
  where
    !bnd = boundsIV vec1

-- | Altertnative to `accumulate` for `IxVector` that share the same bounds.
{-# INLINE accumulateIV #-}
accumulateIV :: (Ix i, U.Unbox i, U.Unbox a, U.Unbox b) => (a -> b -> a) -> IxVector i (U.Vector a) -> IxVector i (U.Vector (i, b)) -> IxVector i (U.Vector a)
accumulateIV !f !vec0 !commands =
  let !input1d = U.map (first (index bnd)) (vecIV commands)
      !vec1d = U.accumulate f (vecIV vec0) input1d
   in IxVector bnd vec1d
  where
    !bnd = boundsIV vec0
    !_ = dbgAssert (boundsIV vec0 == boundsIV commands)

{-# INLINE createIV #-}
createIV :: (G.Vector v a) => (forall s. ST s (IxVector i (G.Mutable v s a))) -> IxVector i (v a)
createIV st = runST $ do
  iv <- st
  let bnd = boundsIV iv
  IxVector bnd <$> G.unsafeFreeze (vecIV iv)

{-# INLINE generateIV #-}
generateIV :: (Unindex i, U.Unbox a) => (i, i) -> (i -> a) -> IxUVector i a
generateIV bnd f = IxVector bnd $ U.generate (rangeSize bnd) (f . unindex bnd)

-- | `U.constructN` for `IxVector`
{-# INLINE constructIV #-}
constructIV :: (Unindex i, U.Unbox a) => (i, i) -> (IxUVector i a -> i -> a) -> IxUVector i a
constructIV bnd f = IxVector bnd $ U.constructN (rangeSize bnd) $ \sofar ->
  f (IxVector bnd sofar) $! unindex bnd (G.length sofar)

{-# INLINE constructMIV #-}
constructMIV :: forall i a m. (Unindex i, PrimMonad m, U.Unbox a) => (i, i) -> (IxUVector i a -> i -> m a) -> m (U.Vector a)
constructMIV bnd@(!_, !_) f = do
  v <- GM.new n
  v' <- G.unsafeFreeze v
  fill v' 0
  where
    !n = rangeSize bnd
    fill :: U.Vector a -> Int -> m (U.Vector a)
    fill !v i
      | i < n = do
          x <- f (IxVector bnd (G.unsafeTake i v)) (unindex bnd i)
          G.elemseq v x $ do
            v' <- G.unsafeThaw v
            GM.unsafeWrite v' i x
            v'' <- G.unsafeFreeze v'
            fill v'' (i + 1)
    fill v _ = return v

{-# INLINE thawIV #-}
thawIV :: (PrimMonad m, G.Vector v a) => IxVector i (v a) -> m (IxVector i (G.Mutable v (PrimState m) a))
thawIV iv = IxVector (boundsIV iv) <$> G.thaw (vecIV iv)

{-# INLINE unsafeThawIV #-}
unsafeThawIV :: (PrimMonad m, G.Vector v a) => IxVector i (v a) -> m (IxVector i (G.Mutable v (PrimState m) a))
unsafeThawIV iv = IxVector (boundsIV iv) <$> G.thaw (vecIV iv)

{-# INLINE freezeIV #-}
freezeIV :: (PrimMonad m, G.Vector v a) => IxVector i (G.Mutable v (PrimState m) a) -> m (IxVector i (v a))
freezeIV iv = IxVector (boundsIV iv) <$> G.freeze (vecIV iv)

{-# INLINE unsafeFreezeIV #-}
unsafeFreezeIV :: (PrimMonad m, G.Vector v a) => IxVector i (G.Mutable v (PrimState m) a) -> m (IxVector i (v a))
unsafeFreezeIV iv = IxVector (boundsIV iv) <$> G.unsafeFreeze (vecIV iv)

-- | Calculates two-dimensional cummulative sum.
--
-- WARNING: Can you really allocate/run \(O(HW)\) algorithm?
--
-- NOTE: Returns a 2D graph with one-based index with a zero row and a column inserted.
--
-- = Typical problems
-- - [ABC 331 D - Tile Pattern](https://atcoder.jp/contests/abc331/tasks/abc331_d)
{-# INLINE csum2D #-}
csum2D :: (HasCallStack, Num a, U.Unbox a) => IxUVector (Int, Int) a -> IxUVector (Int, Int) a
csum2D !gr = IxVector bnd $ U.constructN (rangeSize bnd) $ \sofar -> case unindex bnd (G.length sofar) of
  (0, _) -> 0
  (_, 0) -> 0
  (!y, !x) -> v0 + fromY + fromX - fromD
    where
      -- NOTE: Use zero-based indices on original graph access
      v0 = gr @! (y - 1, x - 1)
      fromY = IxVector bnd sofar @! (y - 1, x)
      fromX = IxVector bnd sofar @! (y, x - 1)
      fromD = IxVector bnd sofar @! (y - 1, x - 1)
  where
    -- Insert the zero row and the column:
    !bnd = second (both (+ 1)) (boundsIV gr)

-- | Returns cummulative sum in the given 2D range.
-- @
-- - - * * *
-- - - * * *
-- = = # # #
-- = = # # #
-- = = # # #
-- @
{-# INLINE (@+!) #-}
(@+!) :: (HasCallStack, Num a, U.Unbox a) => IxUVector (Int, Int) a -> ((Int, Int), (Int, Int)) -> a
(@+!) !csum ((!y1, !x1), (!y2, !x2)) = s1 + s4 - s2 - s3
  where
    -- NOTE: Using one-based indices sinces zeros are inserted
    -- From top left to @#@
    !s1 = csum @! (y2 + 1, x2 + 1)
    -- From top left to @*@
    !s2 = csum @! (y1, x2 + 1)
    -- From top left to @=@
    !s3 = csum @! (y2 + 1, x1)
    -- From top left to @-@
    !s4 = csum @! (y1, x1)

-- | Creates a new `IxVector` with initial value.
{-# INLINE newIV #-}
newIV :: (Ix i, PrimMonad m, U.Unbox a) => (i, i) -> a -> m (IxMUVector (PrimState m) i a)
newIV bnd e0 = IxVector bnd <$> UM.replicate (rangeSize bnd) e0

-- | Reads a value from `IxVector`.
{-# INLINE readIV #-}
readIV :: (HasCallStack, Ix i, PrimMonad m, GM.MVector v a) => IxVector i (v (PrimState m) a) -> i -> m a
readIV IxVector {..} i = GM.read vecIV (index boundsIV i)

-- | Reads a value from `IxVector`.
{-# INLINE readMayIV #-}
readMayIV :: (HasCallStack, Ix i, PrimMonad m, GM.MVector v a) => IxVector i (v (PrimState m) a) -> i -> m (Maybe a)
readMayIV IxVector {..} i
  | not (inRange boundsIV i) = return Nothing
  | otherwise = Just <$> GM.read vecIV (index boundsIV i)

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

{-# INLINE exchangeIV #-}
exchangeIV :: (HasCallStack, Ix i, PrimMonad m, GM.MVector v a) => IxVector i (v (PrimState m) a) -> i -> a -> m a
exchangeIV IxVector {..} i = GM.exchange vecIV (index boundsIV i)

{-# INLINE unsafeExchangeIV #-}
unsafeExchangeIV :: (Ix i, PrimMonad m, GM.MVector v a) => IxVector i (v (PrimState m) a) -> i -> a -> m a
unsafeExchangeIV IxVector {..} i = GM.unsafeExchange vecIV (index boundsIV i)

