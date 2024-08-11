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
import Data.WaveletMatrix.SuccinctDictionary

-- | Wavelet matrix.
data WaveletMatrix = WaveletMatrix
  { -- \(\lceil \log_2 N \rceil\).
    heightWM :: {-# UNPACK #-} !Int,
    -- The length of the original array.
    lengthWM :: {-# UNPACK #-} !Int,
    -- The bit matrix. Each row represents (heightWM - 1 - iRow) bit's on/off.
    bitsWM :: !(V.Vector (U.Vector Bit)),
    -- The number of zeros in each row in the bit matrix.
    nZerosWM :: !(U.Vector Int),
    -- The cummulative sum of bits in each row in words. Each row has length of
    -- \(\lceil N / 64 \rceil + 1\).
    csumsWM :: !(V.Vector (U.Vector Int))
  }
  deriving (Eq, Show)

-- | \(O(N \log N)\) Creates a `WaveletMatrix`.
--
-- - @nx@: The number of different @x@, starting from zero. In other words, every @x@ is in
-- \([0, nx)\).
{-# INLINE newWM #-}
newWM :: Int -> U.Vector Int -> WaveletMatrix
newWM nx xs = runST $ do
  -- TODO: less mutable variables
  orgBits <- UM.replicate (lengthWM * heightWM) $ Bit False
  orgCsum <- UM.replicate (lenCSum * heightWM) (0 :: Int)
  nZeros <- UM.unsafeNew heightWM

  let !bits = V.unfoldrExactN heightWM (UM.splitAt lengthWM) orgBits
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
    nOnes <- csumInPlaceBV csum bitVec'
    GM.unsafeWrite nZeros iRow (lengthWM - nOnes)

    -- preform a stable sort by the bit:
    VAR.sortBy 2 2 (\_ x -> fromEnum (testBit x iBit)) vec

  bitsWM <- V.unfoldrExactN heightWM (U.splitAt lengthWM) <$> U.unsafeFreeze orgBits
  nZerosWM <- U.unsafeFreeze nZeros
  csumsWM <- V.unfoldrExactN heightWM (U.splitAt lenCSum) <$> U.unsafeFreeze orgCsum
  return $ WaveletMatrix {..}
  where
    !lengthWM = G.length xs
    !lenCSum = (lengthWM + wordWM - 1) `div` wordWM + 1 -- +1 for the zero
    -- TODO: use bit operations
    !heightWM =
      let (!h, !_) = until ((>= nx) . snd) (bimap succ (* 2)) (0 :: Int, 1 :: Int)
       in max 1 h

-- * API

-- | \(O(\log a)\) Returns @a[k]@.
accessWM :: WaveletMatrix -> Int -> Int
accessWM WaveletMatrix {..} i0 = res
  where
    (!_, !res) =
      V.ifoldl'
        ( \(!i, !acc) !iRow (!bits, !csum) ->
            let Bit !goRight = G.unsafeIndex bits i
                !i'
                  | goRight = freq1BV bits csum i + G.unsafeIndex nZerosWM iRow
                  | otherwise = freq0BV bits csum i
                !acc'
                  | goRight = setBit acc (heightWM - 1 - iRow)
                  | otherwise = acc
             in (i', acc')
        )
        (i0, 0)
        (V.zip bitsWM csumsWM)

-- | \(O(\log a)\) Returns k-th (0-based) smallest number in [l, r]. Two different values are
-- treated as separate values. Quantile.
kthMinWM :: WaveletMatrix -> Int -> Int -> Int -> Int
kthMinWM WaveletMatrix {..} l_ r_ k_ = res
  where
    (!res, !_, !_, !_) = V.ifoldl' step (0 :: Int, l_, r_ + 1, k_) (V.zip bitsWM csumsWM)
    -- It's binary search over the value range. In each row, we'll focus on either 0 bit values or
    -- 1 bit values in [l, r) and update the range to [l', r').
    step (!acc, !l, !r, !k) !iRow (!bits, !csum)
      -- `r0 - l0`, the number of zeros in [l, r), is bigger than or equal to k:
      -- Go left.
      | k < r0 - l0 = (acc, l0, r0, k)
      -- Go right.
      | otherwise =
          let !acc' = acc .|. bit (heightWM - 1 - iRow)
              !nZeros = G.unsafeIndex nZerosWM iRow
              -- every zero bits come to the left after the move.
              !l' = l + nZeros - l0 -- add the number of zeros in [0, l)
              !r' = r + nZeros - r0 -- add the number of zeros in [0, r)
              !k' = k - (r0 - l0) -- `r0 - l0` zeros go left
           in (acc', l', r', k')
      where
        !l0 = freq0BV bits csum l
        !r0 = freq0BV bits csum r

-- | \(O(\log a)\) Returns k-th (0-based) biggest number in [l, r]. Two different values are
-- treated as separate values.
{-# INLINE kthMaxWM #-}
kthMaxWM :: WaveletMatrix -> Int -> Int -> Int -> Int
kthMaxWM wm l_ r_ k_ = kthMinWM wm l_ r_ (r_ - l_ - k_)

-- | \(O(\log a)\) Returns the number of \(x s.t. x < \mathcal{upper} in [l .. r]\).
freqLTWM :: WaveletMatrix -> Int -> Int -> Int -> Int
freqLTWM WaveletMatrix {..} l_ r_ upper
  | upper >= bit heightWM = r_ + 1 - l_
  | otherwise =
      let (!res, !_, !_) = V.ifoldl' step (0, l_, r_ + 1) $ V.zip bitsWM csumsWM
       in res
  where
    -- [l, r)
    step (!acc, !l, !r) iRow (!bits, !csum) =
      let !b = testBit upper (heightWM - 1 - iRow)
          !l0 = freq0BV bits csum l
          !r0 = freq0BV bits csum r
       in if b
            then (acc + r0 - l0, l - l0 + G.unsafeIndex nZerosWM iRow, r - r0 + G.unsafeIndex nZerosWM iRow)
            else (acc, l0, r0)

-- | \(O(\log a)\) Returns the number of x in [l .. r] in [xl, xr].
{-# INLINE freqInWM #-}
freqInWM :: WaveletMatrix -> Int -> Int -> Int -> Int -> Int
freqInWM wm l_ r_ lx rx = freqLTWM wm l_ r_ (rx + 1) - freqLTWM wm l_ r_ lx

-- | \(O(\log a)\) Returns the number of x in [l .. r].
{-# INLINE freqWM #-}
freqWM :: WaveletMatrix -> Int -> Int -> Int -> Int
freqWM wm l_ r_ x = freqInWM wm l_ r_ x x

-- | \(O(\log a)\) Finds index of @x@. Select.
findIndexWM :: WaveletMatrix -> Int -> Maybe Int
findIndexWM wm = findKthIndexWM wm 0

-- | \(O(\log a)\) Finds kth index of @x@. Select.
findKthIndexWM :: WaveletMatrix -> Int -> Int -> Maybe Int
findKthIndexWM WaveletMatrix {..} k x
  | not (0 <= x && x <= n - 1 && 0 <= k && k <= n - 1) = Nothing
  | otherwise = inner
  where
    !n = G.length (G.head bitsWM)
    inner :: Maybe Int
    inner
      | rEnd <= lEnd + k = Nothing
      -- go up
      | otherwise = res
      where
        -- Go down. Gets the [l, r) range of @x@ in the last array.
        (!lEnd, !rEnd) =
          V.ifoldl'
            ( \(!l, !r) !iRow (!bits, !csum) ->
                let !l0 = freq0BV bits csum l
                    !r0 = freq0BV bits csum r
                 in if testBit x (heightWM - 1 - iRow)
                      then (l - l0 + nZerosWM G.! iRow, r - r0 + nZerosWM G.! iRow)
                      else (l0, r0)
            )
            (0 :: Int, n)
            (V.zip bitsWM csumsWM)
        -- Go up.
        -- FIXME: why maybe?
        !res =
          V.ifoldM'
            ( \ !i !iBit (!bits, !csum) ->
                if testBit x iBit
                  then findKthIndex1BV bits csum $ i - nZerosWM G.! (heightWM - 1 - iBit)
                  else findKthIndex0BV bits csum i
            )
            (lEnd + k)
            (V.zip (V.reverse bitsWM) (V.reverse csumsWM))

-- TODO: prev, next
