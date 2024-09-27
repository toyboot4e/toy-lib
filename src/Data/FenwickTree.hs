{-# LANGUAGE RecordWildCards #-}

-- | TODO: write doc
module Data.FenwickTree where

import Control.Exception (assert)
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bits
import Data.Core.Group
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- | TODO: write doc
data FenwickTree s a = FenwickTree
  { nFT :: {-# UNPACK #-} !Int,
    dataFT :: !(UM.MVector s a)
  }

-- | \(O(N)\) Creates `FenwickTree`.
newFT :: (Monoid a, U.Unbox a, PrimMonad m) => Int -> m (FenwickTree (PrimState m) a)
newFT nFT = do
  dataFT <- UM.replicate nFT mempty
  return FenwickTree {..}

-- | \(O(N \log N)\) Creates `FenwickTree`.
buildFT :: (Monoid a, U.Unbox a, PrimMonad m) => U.Vector a -> m (FenwickTree (PrimState m) a)
buildFT xs = do
  ft <- newFT $ U.length xs
  U.iforM_ xs $ \i x -> do
    addFT ft i x
  return ft

-- | \(O(\log N)\) Calculates the sum in half-open range @[l, r)@.
addFT :: (PrimMonad m, Monoid a, U.Unbox a) => FenwickTree (PrimState m) a -> Int -> a -> m ()
addFT FenwickTree {..} p0 x = do
  let !_ = assert (0 <= p0 && p0 < nFT) ()
  let p1 = p0 + 1
  flip fix p1 $ \loop p -> do
    when (p <= nFT) $ do
      -- FIXME: to unsigned?
      GM.modify dataFT (<> x) (p - 1)
      loop $! p + (p .&. (-p))

-- | \(O(\log N)\) FIXME: write document. Fix one-based?
prefixSumFT :: (PrimMonad m, Monoid a, U.Unbox a) => FenwickTree (PrimState m) a -> Int -> m a
prefixSumFT FenwickTree {..} = inner mempty
  where
    inner !acc !r
      | r <= 0 = return acc
      | otherwise = do
          dx <- GM.read dataFT (r - 1)
          inner (acc <> dx) (r - r .&. (-r))

-- | \(O(\log N)\) Calculates the sum in inclusing range @[l, r]@.
sumFT :: (PrimMonad m, Monoid a, Group a, U.Unbox a) => FenwickTree (PrimState m) a -> Int -> Int -> m a
sumFT ft@FenwickTree {..} l r = do
  let !_ = assert (0 <= l && l <= r && r < nFT) ()
  xr <- prefixSumFT ft $! r + 1
  xl <- prefixSumFT ft l
  return $! xr <> invert xl
