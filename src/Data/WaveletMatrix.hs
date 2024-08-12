{-# LANGUAGE RecordWildCards #-}

-- | Wavelet Matrix with automatic index compression.
module Data.WaveletMatrix where

import Algorithm.Bisect
import Control.Monad
import Data.Maybe
import qualified Data.Vector.Algorithms.Intro as VAI
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import Data.WaveletMatrix.Raw
import GHC.Stack

-- | Wavelet Matrix with automatic index compression.
data WaveletMatrix = WaveletMatrix
  { -- | The wavelet matrix.
    rawWM :: !RawWaveletMatrix,
    -- | Index compression dictionary.
    dictWM :: !(U.Vector Int)
  }

-- | Creates `WaveletMatrix`, internally performing index compression.
newWM :: U.Vector Int -> WaveletMatrix
newWM xs =
  let !dictWM = U.uniq $ U.modify VAI.sort xs
      !xs' = U.map (\x -> fromJust $ bsearchL dictWM (<= x)) xs
      !rawWM = newRWM (G.length xs) xs'
   in WaveletMatrix {..}

-- | \(O(\log a)\) Returns @a[k]@.
--
-- TODO: filter invalid index.
accessWM :: (HasCallStack) => WaveletMatrix -> Int -> Int
accessWM WaveletMatrix {..} i =
  let !x = accessRWM rawWM i
   in dictWM G.! x

-- * kth min (safe)

-- | \(O(\log a)\) Returns k-th (0-based) smallest number in [l, r]. Two different values are
-- treated as separate values. Quantile.
{-# INLINE ikthMinWM #-}
ikthMinWM :: (HasCallStack) => WaveletMatrix -> Int -> Int -> Int -> Maybe (Int, Int)
ikthMinWM WaveletMatrix{..} l_ r_ k_
  | Just (!i, !x) <- ikthMinRWM rawWM l_ r_ k_ = Just (i, dictWM G.! x)
  | otherwise = Nothing

-- | \(O(\log a)\) Returns k-th (0-based) smallest number in [l, r]. Two different values are
-- treated as separate values. Quantile.
{-# INLINE kthMinWM #-}
kthMinWM :: (HasCallStack) => WaveletMatrix -> Int -> Int -> Int -> Maybe Int
kthMinWM WaveletMatrix{..} l_ r_ k_
  | Just !x <- kthMinRWM rawWM l_ r_ k_ = Just $ dictWM G.! x
  | otherwise = Nothing

-- | \(O(\log a)\) Returns k-th (0-based) smallest number in [l, r]. Two different values are
-- treated as separate values. Quantile.
{-# INLINE ikthMaxWM #-}
ikthMaxWM :: (HasCallStack) => WaveletMatrix -> Int -> Int -> Int -> Maybe (Int, Int)
ikthMaxWM WaveletMatrix{..} l_ r_ k_
  | Just (!i, !x) <- ikthMaxRWM rawWM l_ r_ k_ = Just (i, dictWM G.! x)
  | otherwise = Nothing

-- | \(O(\log a)\) Returns k-th (0-based) smallest number in [l, r]. Two different values are
-- treated as separate values. Quantile.
{-# INLINE kthMaxWM #-}
kthMaxWM :: (HasCallStack) => WaveletMatrix -> Int -> Int -> Int -> Maybe Int
kthMaxWM WaveletMatrix{..} l_ r_ k_
  | Just !x <- kthMaxRWM rawWM l_ r_ k_ = Just $ dictWM G.! x
  | otherwise = Nothing

-- * kth min (no boundary check)

-- | \(O(\log a)\)
{-# INLINE unsafeIKthMinWM #-}
unsafeIKthMinWM :: (HasCallStack) => WaveletMatrix -> Int -> Int -> Int -> (Int, Int)
unsafeIKthMinWM WaveletMatrix {..} l_ r_ k_ =
  let (!i, !x) = unsafeIKthMinRWM rawWM l_ r_ k_
   in (i, dictWM G.! x)

-- | \(O(\log a)\)
unsafeKthMinWM :: (HasCallStack) => WaveletMatrix -> Int -> Int -> Int -> Int
unsafeKthMinWM WaveletMatrix {..} l_ r_ k_ =
  let !x = unsafeKthMinRWM rawWM l_ r_ k_
   in dictWM G.! x

-- | \(O(\log a)\)
{-# INLINE unsafeIKthMaxWM #-}
unsafeIKthMaxWM :: (HasCallStack) => WaveletMatrix -> Int -> Int -> Int -> (Int, Int)
unsafeIKthMaxWM WaveletMatrix {..} l_ r_ k_ =
  let (!i, !x) = unsafeIKthMaxRWM rawWM l_ r_ k_
   in (i, dictWM G.! x)

-- | \(O(\log a)\)
{-# INLINE unsafeKthMaxWM #-}
unsafeKthMaxWM :: (HasCallStack) => WaveletMatrix -> Int -> Int -> Int -> Int
unsafeKthMaxWM WaveletMatrix {..} l_ r_ k_ =
  let !x = unsafeKthMinRWM rawWM l_ r_ k_
   in dictWM G.! x

-- * Frequencies

-- | \(O(\log a)\) Returns the number of x in [l .. r] in [xl, xr].
{-# INLINE freqInWM #-}
freqInWM :: WaveletMatrix -> Int -> Int -> Int -> Int -> Int
freqInWM WaveletMatrix {..} l r xl xr
  | not $ 0 <= l && l <= r && r < n = 0
  -- \| not $ 0 <= xl && xl <= xr && xr < n = 0
  | not $ xl <= xr = 0
  | otherwise = freqInRWM rawWM l r xl' xr'
  where
    -- Handles the case @xl@ or  @xr@ is not in the dict
    !n = lengthRWM rawWM
    !xl' = fromMaybe n (bsearchR dictWM (< xl))
    !xr' = fromMaybe (-1) (bsearchL dictWM (<= xr))

-- | \(O(\log a)\) Returns the number of x in [l .. r].
{-# INLINE freqWM #-}
freqWM :: WaveletMatrix -> Int -> Int -> Int -> Int
freqWM wm l r x = freqInWM wm l r x x

-- * Find index

-- | \(O(\log a)\) Finds index of @x@. Select.
{-# INLINE findIndexWM #-}
findIndexWM :: (HasCallStack) => WaveletMatrix -> Int -> Maybe Int
findIndexWM wm = findKthIndexWM wm 0

-- | \(O(\log a)\) Finds kth index of @x@. Select.
{-# INLINE findKthIndexWM #-}
findKthIndexWM :: (HasCallStack) => WaveletMatrix -> Int -> Int -> Maybe Int
findKthIndexWM WaveletMatrix {..} k x = do
  i <- bsearchL dictWM (<= x)
  -- TODO: we don't need such an explicit branch?
  let !x' = dictWM G.! i
  guard $ x' == x
  findKthIndexRWM rawWM k i

lrFindIndexWM :: (HasCallStack) => WaveletMatrix -> Int -> Int -> Int -> Maybe Int
lrFindIndexWM wm = lrFindKthIndexWM wm 0

lrFindKthIndexWM :: (HasCallStack) => WaveletMatrix -> Int -> Int -> Int -> Int -> Maybe Int
lrFindKthIndexWM WaveletMatrix {..} k x l r = do
  i <- bsearchL dictWM (<= x)
  -- TODO: we don't need such an explicit branch?
  let !x' = dictWM G.! i
  guard $ x' == x
  lrFindKthIndexRWM rawWM k i l r

-- * Lookup

-- | \(O(\log a)\) Finds maximum \(x\) in \([l, r]\) s.t. \(x_{ref} \le x\).
{-# INLINE lookupLEWM #-}
lookupLEWM :: (HasCallStack) => WaveletMatrix -> Int -> Int -> Int -> Maybe Int
lookupLEWM wm l r x
  | freq == 0 = Nothing
  | otherwise = Just $ unsafeKthMinWM wm l r (freq - 1)
  where
    -- TODO: minBound works?
    !freq = freqInWM wm l r (minBound `div` 2) x

-- | \(O(\log a)\) Finds maximum \(x\) in \([l, r]\) s.t. \(x_{ref} \lt x\).
{-# INLINE lookupLTWM #-}
lookupLTWM :: (HasCallStack) => WaveletMatrix -> Int -> Int -> Int -> Maybe Int
lookupLTWM wm l r x = lookupLEWM wm l r (x - 1)

-- | \(O(\log a)\) Finds minimum \(x\) in \([l, r]\) s.t. \(x_{ref} \gt x\).
{-# INLINE lookupGEWM #-}
lookupGEWM :: (HasCallStack) => WaveletMatrix -> Int -> Int -> Int -> Maybe Int
lookupGEWM wm l r x
  | freq >= r - l + 1 = Nothing
  | otherwise = Just $ unsafeKthMinWM wm l r freq
  where
    -- TODO: minBound works?
    !freq = freqInWM wm l r (minBound `div` 2) (x - 1)

-- | \(O(\log a)\) Finds minimum \(x\) in \([l, r]\) s.t. \(x_{ref} \ge x\).
{-# INLINE lookupGTWM #-}
lookupGTWM :: (HasCallStack) => WaveletMatrix -> Int -> Int -> Int -> Maybe Int
lookupGTWM wm l r x = lookupGEWM wm l r (x + 1)

-- | \(O(\log A \min(|A|, L))\) Collects \((x, freq)\) in range \([l, r]\) in ascending order of
-- @x@. It's only fast when the domain \(A\) is very small.
{-# INLINE assocsWM #-}
assocsWM :: WaveletMatrix -> Int -> Int -> [(Int, Int)]
assocsWM WaveletMatrix {..} l_ r_ = _assocsWithRWM rawWM l_ r_ (dictWM G.!)

-- | \(O(\log A \min(|A|, L))\) Collects \((x, freq)\) in range \([l, r]\) in ascending order of
-- @x@. It's only fast when the domain \(A\) is very small.
{-# INLINE descAssocsWM #-}
descAssocsWM :: WaveletMatrix -> Int -> Int -> [(Int, Int)]
descAssocsWM WaveletMatrix {..} l_ r_ = _descAssocsWithRWM rawWM l_ r_ (dictWM G.!)

-- TODO: remove HasCallStack
