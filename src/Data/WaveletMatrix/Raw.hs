{-# LANGUAGE RecordWildCards #-}

-- | Wavelet Matrix without index comperssion.
module Data.WaveletMatrix.Raw where

import Control.Monad.ST (runST)
import Data.Bifunctor (bimap)
import Data.Bit
import Data.Bits
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Radix as VAR
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.WaveletMatrix.SuccinctDictionary

-- | Wavelet Matrix without index comperssion.
data RawWaveletMatrix = RawWaveletMatrix
  { -- | \(\lceil \log_2 N \rceil\).
    heightRWM :: {-# UNPACK #-} !Int,
    -- | The length of the original array.
    lengthRWM :: {-# UNPACK #-} !Int,
    -- | The bit matrix. Each row represents (heightRWM - 1 - iRow) bit's on/off.
    bitsRWM :: !(V.Vector (U.Vector Bit)),
    -- | The number of zeros in each row in the bit matrix.
    -- TODO: consider removing it ('cause we have @csumsRWM@). It could be even faster.
    nZerosRWM :: !(U.Vector Int),
    -- | The cummulative sum of bits in each row in words. Each row has length of
    -- \(\lceil N / 64 \rceil + 1\).
    csumsRWM :: !(V.Vector (U.Vector Int))
  }
  deriving (Eq, Show)

-- | \(O(N \log N)\) Creates a `RawWaveletMatrix`.
--
-- - @nx@: The number of different @x@, starting from zero. In other words, every @x@ is in
-- \([0, nx)\).
{-# INLINE newRWM #-}
newRWM :: Int -> U.Vector Int -> RawWaveletMatrix
newRWM nx xs = runST $ do
  -- TODO: less mutable variables
  orgBits <- UM.replicate (lengthRWM * heightRWM) $ Bit False
  orgCsum <- UM.replicate (lenCSum * heightRWM) (0 :: Int)
  nZeros <- UM.unsafeNew heightRWM

  let !bits = V.unfoldrExactN heightRWM (UM.splitAt lengthRWM) orgBits
  let !csums = V.unfoldrExactN heightRWM (UM.splitAt lenCSum) orgCsum

  -- the vector will be sorted by bits.
  vec <- U.thaw xs
  V.iforM_ (V.zip bits csums) $ \iRow (!bitVec, !csum) -> do
    let !iBit = heightRWM - 1 - iRow
    vec' <- U.unsafeFreeze vec
    -- U.imapM_ vec' -- TODO: which is faster, imapM_ vs unsafeWrite?
    U.iforM_ vec' $ \i x -> do
      GM.unsafeWrite bitVec i . Bit $ testBit x iBit

    -- csum.
    GM.unsafeWrite csum 0 (0 :: Int)
    bitVec' <- U.unsafeFreeze bitVec

    -- get popCount by word. TODO: use `castToWords` for most elements
    nOnes <- csumInPlaceBV csum bitVec'
    GM.unsafeWrite nZeros iRow (lengthRWM - nOnes)

    -- preform a stable sort by the bit:
    VAR.sortBy 2 2 (\_ x -> fromEnum (testBit x iBit)) vec

  bitsRWM <- V.unfoldrExactN heightRWM (U.splitAt lengthRWM) <$> U.unsafeFreeze orgBits
  nZerosRWM <- U.unsafeFreeze nZeros
  csumsRWM <- V.unfoldrExactN heightRWM (U.splitAt lenCSum) <$> U.unsafeFreeze orgCsum
  return $ RawWaveletMatrix {..}
  where
    !lengthRWM = G.length xs
    !lenCSum = (lengthRWM + wordWM - 1) `div` wordWM + 1 -- +1 for the zero
    -- TODO: use bit operations
    !heightRWM =
      let (!h, !_) = until ((>= nx) . snd) (bimap succ (* 2)) (0 :: Int, 1 :: Int)
       in max 1 h

-- * API

-- | \(O(\log a)\) Returns @a[k]@.
accessRWM :: RawWaveletMatrix -> Int -> Int
accessRWM RawWaveletMatrix {..} i0 = res
  where
    (!_, !res) =
      V.ifoldl'
        ( \(!i, !acc) !iRow (!bits, !csum) ->
            let Bit !goRight = G.unsafeIndex bits i
                !i'
                  | goRight = freq1BV bits csum i + G.unsafeIndex nZerosRWM iRow
                  | otherwise = freq0BV bits csum i
                !acc'
                  | goRight = setBit acc (heightRWM - 1 - iRow)
                  | otherwise = acc
             in (i', acc')
        )
        (i0, 0)
        (V.zip bitsRWM csumsRWM)

-- | \(O(\log a)\)
_goDownRWM :: RawWaveletMatrix -> Int -> Int -> Int -> (Int, Int, Int, Int)
_goDownRWM RawWaveletMatrix {..} l_ r_ k_ = V.ifoldl' step (0 :: Int, l_, r_ + 1, k_) (V.zip bitsRWM csumsRWM)
  where
    -- It's binary search over the value range. In each row, we'll focus on either 0 bit values or
    -- 1 bit values in [l, r) and update the range to [l', r').
    step (!acc, !l, !r, !k) !iRow (!bits, !csum)
      -- `r0 - l0`, the number of zeros in [l, r), is bigger than or equal to k:
      -- Go left.
      | k < r0 - l0 = (acc, l0, r0, k)
      -- Go right.
      | otherwise =
          let !acc' = acc .|. bit (heightRWM - 1 - iRow)
              !nZeros = G.unsafeIndex nZerosRWM iRow
              -- every zero bits come to the left after the move.
              !l' = l + nZeros - l0 -- add the number of zeros in [0, l)
              !r' = r + nZeros - r0 -- add the number of zeros in [0, r)
              !k' = k - (r0 - l0) -- `r0 - l0` zeros go left
           in (acc', l', r', k')
      where
        !l0 = freq0BV bits csum l
        !r0 = freq0BV bits csum r

-- | \(O(\log a)\)
_goUpRWM :: RawWaveletMatrix -> Int -> Int -> Maybe Int
_goUpRWM RawWaveletMatrix {..} i0 x =
  V.ifoldM'
    ( \ !i !iBit (!bits, !csum) ->
        if testBit x iBit
          then findKthIndex1BV bits csum $ i - nZerosRWM G.! (heightRWM - 1 - iBit)
          else findKthIndex0BV bits csum i
    )
    i0
    (V.zip (V.reverse bitsRWM) (V.reverse csumsRWM))

-- | \(O(\log a)\) Returns k-th (0-based) smallest number in [l, r]. Two different values are
-- treated as separate values. Quantile with index.
kthMinRWM :: RawWaveletMatrix -> Int -> Int -> Int -> Int
kthMinRWM wm l_ r_ k_ =
  let (!x, !_, !_, !_) = _goDownRWM wm l_ r_ k_
   in x

-- | \(O(\log a)\)
ikthMinRWM :: RawWaveletMatrix -> Int -> Int -> Int -> (Int, Int)
ikthMinRWM wm l_ r_ k_ =
  let (!x, !l, !_, !k) = _goDownRWM wm l_ r_ k_
      !i' = fromJust $ _goUpRWM wm (l + k) x
   in (i', x)

-- | \(O(\log a)\) Returns k-th (0-based) biggest number in [l, r]. Two different values are
-- treated as separate values.
{-# INLINE kthMaxRWM #-}
kthMaxRWM :: RawWaveletMatrix -> Int -> Int -> Int -> Int
kthMaxRWM wm l_ r_ k_ = kthMinRWM wm l_ r_ (r_ - l_ - k_)

-- | \(O(\log a)\) Returns the number of \(x s.t. x < \mathcal{upper} in [l .. r]\).
freqLTRWM :: RawWaveletMatrix -> Int -> Int -> Int -> Int
freqLTRWM RawWaveletMatrix {..} l_ r_ upper
  | upper >= bit heightRWM = r_ + 1 - l_
  | otherwise =
      let (!res, !_, !_) = V.ifoldl' step (0, l_, r_ + 1) $ V.zip bitsRWM csumsRWM
       in res
  where
    -- [l, r)
    step (!acc, !l, !r) !iRow (!bits, !csum) =
      let !b = testBit upper (heightRWM - 1 - iRow)
          !l0 = freq0BV bits csum l
          !r0 = freq0BV bits csum r
       in if b
            then (acc + r0 - l0, l - l0 + G.unsafeIndex nZerosRWM iRow, r - r0 + G.unsafeIndex nZerosRWM iRow)
            else (acc, l0, r0)

-- | \(O(\log a)\) Returns the number of x in [l .. r] in [xl, xr].
{-# INLINE freqInRWM #-}
freqInRWM :: RawWaveletMatrix -> Int -> Int -> Int -> Int -> Int
freqInRWM wm l_ r_ lx rx = freqLTRWM wm l_ r_ (rx + 1) - freqLTRWM wm l_ r_ lx

-- | \(O(\log a)\) Returns the number of x in [l .. r].
{-# INLINE freqRWM #-}
freqRWM :: RawWaveletMatrix -> Int -> Int -> Int -> Int
freqRWM wm l_ r_ x = freqInRWM wm l_ r_ x x

-- | \(O(\log a)\) Finds index of @x@. Select.
findIndexRWM :: RawWaveletMatrix -> Int -> Maybe Int
findIndexRWM wm = findKthIndexRWM wm 0

-- | \(O(\log a)\) Finds kth index of @x@. Select.
findKthIndexRWM :: RawWaveletMatrix -> Int -> Int -> Maybe Int
findKthIndexRWM wm@RawWaveletMatrix {..} k x
  | not (0 <= x && x <= n - 1 && 0 <= k && k <= n - 1) = Nothing
  | otherwise = inner
  where
    !n = G.length (G.head bitsRWM)
    inner :: Maybe Int
    inner
      | rEnd <= lEnd + k = Nothing
      -- go up
      | otherwise = _goUpRWM wm (lEnd + k) x
      where
        -- TODO: replace with _goDownRWM
        -- Go down. Gets the [l, r) range of @x@ in the last array.
        (!lEnd, !rEnd) =
          V.ifoldl'
            ( \(!l, !r) !iRow (!bits, !csum) ->
                let !l0 = freq0BV bits csum l
                    !r0 = freq0BV bits csum r
                 in if testBit x (heightRWM - 1 - iRow)
                      then (l - l0 + nZerosRWM G.! iRow, r - r0 + nZerosRWM G.! iRow)
                      else (l0, r0)
            )
            (0 :: Int, n)
            (V.zip bitsRWM csumsRWM)

-- TODO: prev, next
-- TODO: compress

