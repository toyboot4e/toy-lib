{-# LANGUAGE RecordWildCards #-}

-- | Wavelet Matrix.
module Data.WaveletMatrix where

import Control.Monad.ST (runST)
import Data.Bifunctor (bimap)
import Data.Bit
import Data.Bits
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Radix as VAR
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Stack

-- | Wavelet matrix.
data WaveletMatrix = WaveletMatrix
  { -- \(\lceil \log_2 N \rceil\).
    heightWM :: {-# UNPACK #-} !Int,
    -- The bit matrix. Each row represents (heightWM - 1 - iRow) bit's on/off.
    bitsWM :: !(V.Vector (U.Vector Bit)),
    -- The number of zeros in each row in the bit matrix.
    nZerosWM :: !(U.Vector Int),
    -- The cummulative sum of bits in each row in words. Each row has length of
    -- \(\lceil N / 64 \rceil + 1\).
    csumsWM :: !(V.Vector (U.Vector Int))
  }
  deriving (Eq, Show)

-- | Block sized for the internal cumultaive sum.
{-# INLINE wordWM #-}
wordWM :: Int
wordWM = 64

-- | \(O(N)\) Creates a `WaveletMatrix`.
--
-- - @nx@: The number of different @x@, starting from zero. In other words, every @x@ is in
-- \([0, nx)\).
{-# INLINE newWM #-}
newWM :: Int -> U.Vector Int -> WaveletMatrix
newWM nx xs = runST $ do
  -- TODO: less mutable variables
  orgBits <- UM.replicate (n * heightWM) $ Bit False
  orgCsum <- UM.replicate (lenCSum * heightWM) (0 :: Int)
  nZeros <- UM.unsafeNew heightWM

  let !bits = V.unfoldrExactN heightWM (UM.splitAt n) orgBits
  let !csums = V.unfoldrExactN heightWM (UM.splitAt lenCSum) orgCsum

  -- the vector will be sorted by bits.
  vec <- U.thaw xs
  V.iforM_ (V.zip bits csums) $ \iRow (!bitVec, !csum) -> do
    let !iBit = heightWM - 1 - iRow
    vec' <- U.unsafeFreeze vec
    -- U.imapM_ vec' -- TODO: which is faster, imapM_ vs unsafeWrite?
    U.iforM_ vec' $ \i x -> do
      GM.unsafeWrite bitVec i . Bit $ testBit x iBit

    -- csum.
    GM.unsafeWrite csum 0 (0 :: Int)
    bitVec' <- U.unsafeFreeze bitVec

    -- get popCount by word. TODO: use `castToWords` for most elements
    nOnes <-
      U.ifoldM'
        ( \ !acc i wordSum -> do
            let !acc' = acc + wordSum
            GM.unsafeWrite csum (i + 1) acc'
            return acc'
        )
        (0 :: Int)
        $ U.unfoldrExactN
          (lenCSum - 1)
          (\bits' -> (popCount (U.take wordWM bits'), U.drop wordWM bits'))
          bitVec'

    GM.unsafeWrite nZeros iRow (n - nOnes)

    -- preform a stable sort by the bit:
    VAR.sortBy 2 2 (\_ x -> fromEnum (testBit x iBit)) vec

  bitsWM <- V.unfoldrExactN heightWM (U.splitAt n) <$> U.unsafeFreeze orgBits
  nZerosWM <- U.unsafeFreeze nZeros
  csumsWM <- V.unfoldrExactN heightWM (U.splitAt lenCSum) <$> U.unsafeFreeze orgCsum
  return $ WaveletMatrix {..}
  where
    !n = G.length xs
    !lenCSum = (n + wordWM - 1) `div` wordWM + 1 -- +1 for the zero
    -- TODO: use bit operations
    (!heightWM, !_) = until ((>= nx) . snd) (bimap succ (* 2)) (0 :: Int, 1 :: Int)

-- | \(O(1)\) Counts the number of 0 bits in interval [0, i].
{-# INLINE rank0WM #-}
rank0WM :: (HasCallStack) => WaveletMatrix -> Int -> Int -> Int
rank0WM wm iRow = _rank0BV (bitsWM wm G.! iRow) (csumsWM wm G.! iRow)

-- | \(O(1)\) Counts the number of 1 bits in interval [0, i].
{-# INLINE rank1WM #-}
rank1WM :: (HasCallStack) => WaveletMatrix -> Int -> Int -> Int
rank1WM wm iRow = _rank1BV (bitsWM wm G.! iRow) (csumsWM wm G.! iRow)

-- \(O(1)\) | Counts the number of 0 bits in interval [0, i). The input is a cumulative sum of bit vector by word.
{-# INLINE _rank0BV #-}
_rank0BV :: (HasCallStack) => U.Vector Bit -> U.Vector Int -> Int -> Int
_rank0BV bits bitsCSum i = i - _rank1BV bits bitsCSum i

-- | \(O(1)\) Counts the number of 1 bits in interval [0, i). The input is a cumulative sum of bit vector by word.
{-# INLINE _rank1BV #-}
_rank1BV :: (HasCallStack) => U.Vector Bit -> U.Vector Int -> Int -> Int
_rank1BV bits bitsCSum i = fromCSum + fromRest
  where
    (!nWords, !nRest) = i `divMod` wordWM
    fromCSum = bitsCSum G.! nWords
    fromRest = popCount . U.take nRest . U.drop (nWords * wordWM) $ bits

-- | \(O(\log a)\) Returns @a[k]@.
accessWM :: (HasCallStack) => WaveletMatrix -> Int -> Int
accessWM WaveletMatrix {..} i0 = res
  where
    (!_, !res) =
      V.ifoldl'
        ( \(!i, !acc) !iRow (!bits, !csum) ->
            let Bit !b = G.unsafeIndex bits i
                !i' = if b then _rank1BV bits csum i + (nZerosWM G.! iRow) else _rank0BV bits csum i
                !acc' = if b then setBit acc (heightWM - 1 - iRow) else acc
             in (i', acc')
        )
        (i0, 0)
        -- TODO: indexing can be faster
        (V.zip bitsWM csumsWM)

-- | \(O(\log a)\) Returns k-th (0-based) smallest number in [l, r]. Two different values are
-- treated as separate values.
--
-- TODO: Return with index?
kthSmallestWM :: (HasCallStack) => WaveletMatrix -> Int -> Int -> Int -> Int
kthSmallestWM WaveletMatrix {..} l_ r_ k_ = res
  where
    (!res, !_, !_, !_) = V.ifoldl' step (0 :: Int, l_, r_ + 1, k_) (V.zip bitsWM csumsWM)
    -- Here it's half-open section [l, r).
    step (!acc, !l, !r, !k) iRow (!bits, !csum)
      -- `r0 - l0`: the number of zeros in [l, r) is bigger than or equal to k: go left.
      | k < r0 - l0 = (acc, l0, r0, k)
      -- go right.
      | otherwise =
          let !acc' = acc .|. bit (heightWM - 1 - iRow)
              !nZeros = nZerosWM G.! iRow
              -- every zero bits come to the left after the move.
              !l' = l + nZeros - l0 -- add the number of zeros in [0, l)
              !r' = r + nZeros - r0 -- add the number of zeros in [0, r)
              !k' = k - (r0 - l0) -- `r0 - l0` zeros go left
           in (acc', l', r', k')
      where
        !l0 = _rank0BV bits csum l
        !r0 = _rank0BV bits csum r
