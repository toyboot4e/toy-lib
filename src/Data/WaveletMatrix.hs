{-# LANGUAGE RecordWildCards #-}

-- | Wavelet Matrix with automatic index compression.
module Data.WaveletMatrix where

import Algorithm.Bisect
import Data.Maybe
import qualified Data.Vector.Algorithms.Intro as VAI
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import Data.WaveletMatrix.Raw
import GHC.Stack

data WaveletMatrix = WaveletMatrix
  { -- | The wavelet matrix.
    rawWM :: !RawWaveletMatrix,
    -- | Index compression dictionary.
    dictWM :: !(U.Vector Int)
  }

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

-- | \(O(\log a)\) Returns k-th (0-based) smallest number in [l, r]. Two different values are
-- treated as separate values. Quantile.
kthMinWM :: (HasCallStack) => WaveletMatrix -> Int -> Int -> Int -> Int
kthMinWM WaveletMatrix {..} l_ r_ k_ =
  let !x = kthMinRWM rawWM l_ r_ k_
   in dictWM G.! x

-- | \(O(\log a)\) Returns k-th (0-based) smallest number in [l, r]. Two different values are
-- treated as separate values. Quantile with index.
ikthMinWM :: (HasCallStack) => WaveletMatrix -> Int -> Int -> Int -> (Int, Int)
ikthMinWM WaveletMatrix {..} l_ r_ k_ =
  let (!i, !x) = ikthMinRWM rawWM l_ r_ k_
   in (i, dictWM G.! x)

-- | \(O(\log a)\) Returns k-th (0-based) biggest number in [l, r]. Two different values are
-- treated as separate values.
{-# INLINE kthMaxWM #-}
kthMaxWM :: (HasCallStack) => WaveletMatrix -> Int -> Int -> Int -> Int
kthMaxWM WaveletMatrix {..} l_ r_ k_ =
  let !x = kthMinRWM rawWM l_ r_ k_
   in dictWM G.! x

-- | \(O(\log a)\) Returns the number of x in [l .. r] in [xl, xr].
{-# INLINE freqInWM #-}
freqInWM :: WaveletMatrix -> Int -> Int -> Int -> Int -> Int
freqInWM WaveletMatrix {..} l r xl xr
  -- FIXME: duplicate bound check
  | 0 <= xl' && xl' <= xr' && xr' < n = freqInRWM rawWM l r xl' xr'
  | otherwise = 0
  where
    -- Handles the case @xl@ or  @xr@ is not in the dict
    !n = lengthRWM rawWM
    !xl' = fromMaybe n (bsearchR dictWM (< xl))
    !xr' = fromMaybe (-1) (bsearchL dictWM (<= xr))

-- | \(O(\log a)\) Returns the number of x in [l .. r].
{-# INLINE freqWM #-}
freqWM :: (HasCallStack) => WaveletMatrix -> Int -> Int -> Int -> Int
freqWM wm l r x = freqInWM wm l r x x

-- | \(O(\log a)\) Finds index of @x@. Select.
{-# INLINE findIndexWM #-}
findIndexWM :: (HasCallStack) => WaveletMatrix -> Int -> Maybe Int
findIndexWM wm = findKthIndexWM wm 0

-- | \(O(\log a)\) Finds kth index of @x@. Select.
{-# INLINE findKthIndexWM #-}
findKthIndexWM :: (HasCallStack) => WaveletMatrix -> Int -> Int -> Maybe Int
findKthIndexWM WaveletMatrix {..} k x
  | Just i <- bsearchL dictWM (<= x) =
      -- TODO: we don't need such an explicit branch?
      if dictWM G.! i == x
        then findKthIndexRWM rawWM k i
        else Nothing
  | otherwise = Nothing

-- TODO: remove HasCallStack
