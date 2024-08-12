-- | Succinct indexable dictionary functions.
module Data.WaveletMatrix.SuccinctDictionary where

import Algorithm.Bisect
import Control.Monad.Primitive
import Data.Bit
import Data.Bits
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- | Block sized for the internal cumultaive sum.
{-# INLINE wordWM #-}
wordWM :: Int
wordWM = 64

-- | \(O(N)\) Calculates the cumulative sum by word for the bit vector in-place.
csumInPlaceBV :: (PrimMonad m) => UM.MVector (PrimState m) Int -> U.Vector Bit -> m Int
csumInPlaceBV csum bits = do
  GM.unsafeWrite csum 0 (0 :: Int)

  -- Calcuate popCount by word. TODO: use `castToWords` for most elements
  U.ifoldM'
    ( \ !acc i wordSum -> do
        let !acc' = acc + wordSum
        GM.unsafeWrite csum (i + 1) acc'
        return acc'
    )
    (0 :: Int)
    $ U.unfoldrExactN
      (GM.length csum - 1)
      (\bits' -> (popCount (U.take wordWM bits'), U.drop wordWM bits'))
      bits

-- | \(O(N)\) Calculates the cumulative sum by word for the bit vector.
csumBV :: U.Vector Bit -> U.Vector Int
csumBV bits = U.create $ do
  let !lenCSum = (G.length bits + wordWM - 1) `div` wordWM + 1
  vec <- UM.replicate lenCSum (0 :: Int)
  _ <- csumInPlaceBV vec bits
  return vec

-- | \(O(1)\) Counts the number of 0 bits in interval [0, i). The input is a cumulative sum of bit
-- vector by word. Rank 0.
{-# INLINE freq0BV #-}
freq0BV :: U.Vector Bit -> U.Vector Int -> Int -> Int
freq0BV bits bitsCSum i = i - freq1BV bits bitsCSum i

-- | \(O(1)\) Counts the number of 1 bits in interval [0, i). The input is a cumulative sum of bit
-- vector by word. Rank 1.
{-# INLINE freq1BV #-}
freq1BV :: U.Vector Bit -> U.Vector Int -> Int -> Int
freq1BV bits bitsCSum i = fromCSum + fromRest
  where
    -- TODO: check bounds for i?
    (!nWords, !nRest) = i `divMod` wordWM
    fromCSum = G.unsafeIndex bitsCSum nWords
    fromRest = popCount . U.take nRest . U.drop (nWords * wordWM) $ bits

-- | \(O(\log N)\) Finds the index of kth @0@. Select 0.
{-# INLINE findKthIndex0BV #-}
findKthIndex0BV ::  U.Vector Bit -> U.Vector Int -> Int -> Maybe Int
findKthIndex0BV bits bitsCSum k = lrFindKthIndex0BV bits bitsCSum k 0 (G.length bits - 1)

-- | \(O(\log N)\) Finds the index of kth @1@. Select 1.
{-# INLINE findKthIndex1BV #-}
findKthIndex1BV ::  U.Vector Bit -> U.Vector Int -> Int -> Maybe Int
findKthIndex1BV bits bitsCSum k = lrFindKthIndex1BV bits bitsCSum k 0 (G.length bits -1)

-- | \(O(\log N)\) Finds the index of kth @0@ in [l, r]. Select 0.
lrFindKthIndex0BV ::  U.Vector Bit -> U.Vector Int -> Int -> Int -> Int -> Maybe Int
lrFindKthIndex0BV bits bitsCSum k l r
  -- TODO: use nZeros for filtering?
  | k < 0 = Nothing
  | nZeros <= k = Nothing
  | otherwise = bisectL l (r + 1) $ \i -> freq0BV bits bitsCSum i - l0 < k + 1
  where
    nZeros = freq0BV bits bitsCSum (r + 1) - l0
    l0 = freq0BV bits bitsCSum l

-- | \(O(\log N)\) Finds the index of kth @1@ in [l, r]. Select 1.
lrFindKthIndex1BV ::  U.Vector Bit -> U.Vector Int -> Int -> Int -> Int -> Maybe Int
lrFindKthIndex1BV bits bitsCSum k l r
  -- TODO: use nOnes for filtering?
  | k < 0 = Nothing
  | nOnes <= k = Nothing
  | otherwise = bisectL l (r + 1) $ \i -> freq1BV bits bitsCSum i - l1 < k + 1
  where
    nOnes = freq1BV bits bitsCSum (r + 1) - l1
    l1 = freq1BV bits bitsCSum l
