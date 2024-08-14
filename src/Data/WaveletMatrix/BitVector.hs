{-# LANGUAGE RecordWildCards #-}

-- | Bit vector (in the context of wavelet matrix) is a collection of bits that can handle @rank@
-- (@freq@) in \(O(1)\) and @select@ (@findIndex@) in \(O(\log N)\) without much memory.
--
-- There are two popular ways for implementing such a bit vector.
--
-- = Word-based cumulative sum
--
-- One way to implement such a bit vector is to use a cumulative sum of bits by words (& bytes).
-- This module is implemented with that.
--
-- = Succinct bit vector
--
-- Another way to implement a bit vector with less memory is using a succinct bit vector. Unless
-- large $N$ problems come, I might not implement it.
module Data.WaveletMatrix.BitVector where

import Algorithm.Bisect
import Control.Monad.Primitive
import Data.Bit
import Data.Bits
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- | Block sized for the internal cumultaive sum in the bit vector.
{-# INLINE wordWM #-}
wordWM :: Int
wordWM = 64

-- | Bit vector is a collection of bits that can handle @rank@ (@freq@) in \(O(1)\) and @select@
-- (@findIndex@) in \(O(N)\).
data BitVector = BitVector
  { -- | Bits.
    bitsBV :: !(U.Vector Bit),
    -- | Cumulative sum of bits by words.
    csumBV :: !(U.Vector Int)
  }
  deriving (Eq, Show)

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
newCSumBV :: U.Vector Bit -> U.Vector Int
newCSumBV bits = U.create $ do
  let !lenCSum = (G.length bits + wordWM - 1) `div` wordWM + 1
  vec <- UM.replicate lenCSum (0 :: Int)
  _ <- csumInPlaceBV vec bits
  return vec

-- | \(O(1)\) Counts the number of 0 bits in interval [0, i). The input is a cumulative sum of bit
-- vector by word. Rank 0.
{-# INLINE freq0BV #-}
freq0BV :: BitVector -> Int -> Int
freq0BV bv i = i - freq1BV bv i

-- | \(O(1)\) Counts the number of 1 bits in interval [0, i). The input is a cumulative sum of bit
-- vector by word. Rank 1.
{-# INLINE freq1BV #-}
freq1BV :: BitVector -> Int -> Int
freq1BV BitVector{..} i = fromCSum + fromRest
  where
    -- TODO: check bounds for i?
    (!nWords, !nRest) = i `divMod` wordWM
    fromCSum = G.unsafeIndex csumBV nWords
    fromRest = popCount . U.take nRest . U.drop (nWords * wordWM) $ bitsBV

-- | \(O(\log N)\) Finds the index of kth @0@. Select 0.
{-# INLINE findKthIndex0BV #-}
findKthIndex0BV ::  BitVector -> Int -> Maybe Int
findKthIndex0BV bv k = lrFindKthIndex0BV bv k 0 (G.length (bitsBV bv) - 1)

-- | \(O(\log N)\) Finds the index of kth @1@. Select 1.
{-# INLINE findKthIndex1BV #-}
findKthIndex1BV ::  BitVector -> Int -> Maybe Int
findKthIndex1BV bv k = lrFindKthIndex1BV bv k 0 (G.length (bitsBV bv) -1)

-- | \(O(\log N)\) Finds the index of kth @0@ in [l, r]. Select 0.
lrFindKthIndex0BV ::  BitVector -> Int -> Int -> Int -> Maybe Int
lrFindKthIndex0BV bv k l r
  -- TODO: use nZeros for filtering?
  | k < 0 = Nothing
  | nZeros <= k = Nothing
  | otherwise = bisectL l (r + 1) $ \i -> freq0BV bv i - l0 < k + 1
  where
    nZeros = freq0BV bv (r + 1) - l0
    l0 = freq0BV bv l

-- | \(O(\log N)\) Finds the index of kth @1@ in [l, r]. Select 1.
lrFindKthIndex1BV ::  BitVector -> Int -> Int -> Int -> Maybe Int
lrFindKthIndex1BV bv k l r
  -- TODO: use nOnes for filtering?
  | k < 0 = Nothing
  | nOnes <= k = Nothing
  | otherwise = bisectL l (r + 1) $ \i -> freq1BV bv i - l1 < k + 1
  where
    nOnes = freq1BV bv (r + 1) - l1
    l1 = freq1BV bv l
