{-# LANGUAGE RecordWildCards #-}

-- | Wavelet Matrix without index comperssion.
--
-- https://miti-7.hatenablog.com/entry/2018/04/28/152259
module Data.WaveletMatrix.Raw where

-- TODO: use y, instead of x

import Control.Monad
import Control.Monad.ST (runST)
import Control.Monad.Trans.State.Strict (execState, modify')
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
import Data.WaveletMatrix.BitVector

-- | Wavelet Matrix without index comperssion.
data RawWaveletMatrix = RawWaveletMatrix
  { -- | \(\lceil \log_2 N \rceil\).
    heightRWM :: {-# UNPACK #-} !Int,
    -- | The length of the original array.
    lengthRWM :: {-# UNPACK #-} !Int,
    -- | The bit matrix. Each row represents (heightRWM - 1 - iRow) bit's on/off. It takes memory
    -- space of \(N\) bytes.
    bitsRWM :: !(V.Vector BitVector),
    -- | The number of zeros in each row in the bit matrix.
    -- TODO: consider removing it ('cause we have @csumsRWM@). It could be even faster.
    nZerosRWM :: !(U.Vector Int)
  }
  deriving (Eq, Show)

-- | \(O(N \log N)\) Creates a `RawWaveletMatrix`.
--
-- - @nx@: The number of different @x@, starting from zero. In other words, every @x@ is in
-- \([0, nx)\).
{-# INLINE buildRWM #-}
buildRWM :: Int -> U.Vector Int -> RawWaveletMatrix
buildRWM nx xs = runST $ do
  -- TODO: less mutable variables
  orgBits <- UM.replicate (lengthRWM * heightRWM) $ Bit False
  orgCsum <- UM.replicate (lenCSum * heightRWM) (0 :: Int)
  nZeros <- UM.unsafeNew heightRWM

  -- views by row over the contiguous memory:
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

  nZerosRWM <- U.unsafeFreeze nZeros
  bits' <- V.unfoldrExactN heightRWM (U.splitAt lengthRWM) <$> U.unsafeFreeze orgBits
  csums' <- V.unfoldrExactN heightRWM (U.splitAt lenCSum) <$> U.unsafeFreeze orgCsum
  let !bitsRWM = V.zipWith BitVector bits' csums'
  pure $ RawWaveletMatrix {..}
  where
    !lengthRWM = G.length xs
    !lenCSum = (lengthRWM + wordWM - 1) `div` wordWM + 1 -- +1 for the zero
    -- TODO: use bit operations
    (!heightRWM, !_) = until ((>= nx) . snd) (bimap succ (* 2)) (1 :: Int, 2 :: Int)

-- | \(O(\log a)\) Returns @a[k]@.
--
-- TODO: Return `Maybe`?
{-# INLINE accessRWM #-}
accessRWM :: RawWaveletMatrix -> Int -> Int
accessRWM RawWaveletMatrix {..} i0 = res
  where
    (!_, !res) =
      V.ifoldl'
        ( \(!i, !acc) !iRow !bits ->
            let Bit !goRight = G.unsafeIndex (bitsBV bits) i
                !i'
                  | goRight = freq1BV bits i + G.unsafeIndex nZerosRWM iRow
                  | otherwise = freq0BV bits i
                !acc'
                  | goRight = setBit acc (heightRWM - 1 - iRow)
                  | otherwise = acc
             in (i', acc')
        )
        (i0, 0)
        bitsRWM

-- * kth min (safe)

-- | \(O(\log a)\) Goes down the wavelet matrix for collecting the kth minimum value.
{-# INLINE _goDownRWM #-}
_goDownRWM :: RawWaveletMatrix -> Int -> Int -> Int -> (Int, Int, Int, Int)
_goDownRWM RawWaveletMatrix {..} l_ r_ k_ = V.ifoldl' step (0 :: Int, l_, r_ + 1, k_) bitsRWM
  where
    -- It's binary search over the value range. In each row, we'll focus on either 0 bit values or
    -- 1 bit values in [l, r) and update the range to [l', r').
    step (!acc, !l, !r, !k) !iRow !bits
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
        !l0 = freq0BV bits l
        !r0 = freq0BV bits r

-- | \(O(\log a)\) Goes up the wavelet matrix for collecting the value @x@.
{-# INLINE _goUpRWM #-}
_goUpRWM :: RawWaveletMatrix -> Int -> Int -> Maybe Int
_goUpRWM RawWaveletMatrix {..} i0 x =
  V.ifoldM'
    ( \ !i !iBit !bits ->
        if testBit x iBit
          then findKthIndex1BV bits $ i - nZerosRWM G.! (heightRWM - 1 - iBit)
          else findKthIndex0BV bits i
    )
    i0
    (V.reverse bitsRWM)

-- | \(O(\log a)\) Returns k-th (0-based) smallest number in [l, r]. Two different values are
-- treated as separate values. Quantile with index.
{-# INLINE kthMinRWM #-}
kthMinRWM :: RawWaveletMatrix -> Int -> Int -> Int -> Maybe Int
kthMinRWM wm l_ r_ k_
  | k_ < 0 || k_ > r_ - l_ = Nothing
  | otherwise = Just $ unsafeKthMinRWM wm l_ r_ k_

-- | \(O(\log a)\)
{-# INLINE ikthMinRWM #-}
ikthMinRWM :: RawWaveletMatrix -> Int -> Int -> Int -> Maybe (Int, Int)
ikthMinRWM wm l_ r_ k_
  | k_ < 0 || k_ > r_ - l_ = Nothing
  | otherwise = Just $ unsafeIKthMinRWM wm l_ r_ k_

-- | \(O(\log a)\) Returns k-th (0-based) biggest number in [l, r]. Two different values are
-- treated as separate values.
{-# INLINE kthMaxRWM #-}
kthMaxRWM :: RawWaveletMatrix -> Int -> Int -> Int -> Maybe Int
kthMaxRWM wm l_ r_ k_
  | k_ < 0 || k_ > r_ - l_ = Nothing
  | otherwise = Just $ unsafeKthMaxRWM wm l_ r_ k_

-- | \(O(\log a)\)
{-# INLINE ikthMaxRWM #-}
ikthMaxRWM :: RawWaveletMatrix -> Int -> Int -> Int -> Maybe (Int, Int)
ikthMaxRWM wm l_ r_ k_
  | k_ < 0 || k_ > r_ - l_ = Nothing
  | otherwise = Just $ unsafeIKthMaxRWM wm l_ r_ k_

-- * kth min (no boundary check)

-- | \(O(\log a)\) Returns k-th (0-based) smallest number in [l, r]. Two different values are
-- treated as separate values. Quantile with index.
{-# INLINE unsafeKthMinRWM #-}
unsafeKthMinRWM :: RawWaveletMatrix -> Int -> Int -> Int -> Int
unsafeKthMinRWM wm l_ r_ k_ =
  let (!x, !_, !_, !_) = _goDownRWM wm l_ r_ k_
   in x

-- | \(O(\log a)\)
{-# INLINE unsafeIKthMinRWM #-}
unsafeIKthMinRWM :: RawWaveletMatrix -> Int -> Int -> Int -> (Int, Int)
unsafeIKthMinRWM wm l_ r_ k_ =
  let (!x, !l, !_, !k) = _goDownRWM wm l_ r_ k_
      !i' = fromJust $ _goUpRWM wm (l + k) x
   in (i', x)

-- | \(O(\log a)\) Returns k-th (0-based) biggest number in [l, r]. Two different values are
-- treated as separate values.
{-# INLINE unsafeKthMaxRWM #-}
unsafeKthMaxRWM :: RawWaveletMatrix -> Int -> Int -> Int -> Int
unsafeKthMaxRWM wm l_ r_ k_ = unsafeKthMinRWM wm l_ r_ (r_ - l_ - k_)

-- | \(O(\log a)\)
{-# INLINE unsafeIKthMaxRWM #-}
unsafeIKthMaxRWM :: RawWaveletMatrix -> Int -> Int -> Int -> (Int, Int)
unsafeIKthMaxRWM wm l_ r_ k_ = unsafeIKthMinRWM wm l_ r_ (r_ - l_ - k_)

-- * Freq

-- | \(O(\log a)\) Returns the number of \(x s.t. x < \mathcal{upper} in [l .. r]\).
{-# INLINE freqLTRWM #-}
freqLTRWM :: RawWaveletMatrix -> Int -> Int -> Int -> Int
freqLTRWM RawWaveletMatrix {..} l_ r_ upper
  -- REMARK: This is required. The function below cannot handle the case N = 2^i and upper = N.
  | upper >= bit heightRWM = r_ + 1 - l_
  | otherwise =
      let (!res, !_, !_) = V.ifoldl' step (0, l_, r_ + 1) bitsRWM
       in res
  where
    -- [l, r)
    step (!acc, !l, !r) !iRow !bits =
      let !b = testBit upper (heightRWM - 1 - iRow)
          !l0 = freq0BV bits l
          !r0 = freq0BV bits r
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

-- * Find index

-- | \(O(\log a)\) Finds index of @x@. Select.
{-# INLINE findIndexRWM #-}
findIndexRWM :: RawWaveletMatrix -> Int -> Maybe Int
findIndexRWM wm = findKthIndexRWM wm 0

-- | \(O(\log a)\) Finds kth index of @x@. Select.
{-# INLINE findKthIndexRWM #-}
findKthIndexRWM :: RawWaveletMatrix -> Int -> Int -> Maybe Int
findKthIndexRWM wm k x = lrFindKthIndexRWM wm k x 0 (lengthRWM wm - 1)

-- | \(O(\log a)\) Finds index of @x@. Select.
{-# INLINE lrFindIndexRWM #-}
lrFindIndexRWM :: RawWaveletMatrix -> Int -> Int -> Int -> Maybe Int
lrFindIndexRWM wm = lrFindKthIndexRWM wm 0

-- | \(O(\log a)\) Finds kth index of @x@. Select.
{-# INLINE lrFindKthIndexRWM #-}
lrFindKthIndexRWM :: RawWaveletMatrix -> Int -> Int -> Int -> Int -> Maybe Int
lrFindKthIndexRWM wm@RawWaveletMatrix {..} k x l_ r_
  | not (0 <= x && x <= n - 1 && 0 <= k && k <= n - 1) = Nothing
  | otherwise = inner
  where
    !n = lengthRWM
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
            ( \(!l, !r) !iRow !bits ->
                let !l0 = freq0BV bits l
                    !r0 = freq0BV bits r
                 in if testBit x (heightRWM - 1 - iRow)
                      then (l + nZerosRWM G.! iRow - l0, r + nZerosRWM G.! iRow - r0)
                      else (l0, r0)
            )
            (l_, r_ + 1)
            bitsRWM

-- * Lookup

-- | \(O(\log a)\) Finds maximum \(x\) in \([l, r]\) s.t. \(x_{ref} \le x\).
{-# INLINE lookupLERWM #-}
lookupLERWM :: RawWaveletMatrix -> Int -> Int -> Int -> Maybe Int
lookupLERWM wm l r x
  | freq == 0 = Nothing
  | otherwise = Just $ unsafeKthMinRWM wm l r (freq - 1)
  where
    -- TODO: minBound works?
    !freq = freqInRWM wm l r (minBound `div` 2) x

-- | \(O(\log a)\) Finds maximum \(x\) in \([l, r]\) s.t. \(x_{ref} \lt x\).
{-# INLINE lookupLTRWM #-}
lookupLTRWM :: RawWaveletMatrix -> Int -> Int -> Int -> Maybe Int
lookupLTRWM wm l r x = lookupLERWM wm l r (x - 1)

-- | \(O(\log a)\) Finds minimum \(x\) in \([l, r]\) s.t. \(x_{ref} \gt x\).
{-# INLINE lookupGERWM #-}
lookupGERWM :: RawWaveletMatrix -> Int -> Int -> Int -> Maybe Int
lookupGERWM wm l r x
  | freq >= r - l + 1 = Nothing
  | otherwise = Just $ unsafeKthMinRWM wm l r freq
  where
    -- TODO: minBound works?
    !freq = freqInRWM wm l r (minBound `div` 2) (x - 1)

-- | \(O(\log a)\) Finds minimum \(x\) in \([l, r]\) s.t. \(x_{ref} \ge x\).
{-# INLINE lookupGTRWM #-}
lookupGTRWM :: RawWaveletMatrix -> Int -> Int -> Int -> Maybe Int
lookupGTRWM wm l r x = lookupGERWM wm l r (x + 1)

-- * Associations of (freq, value)

-- FIXME: assocs is for (x, freq) rather than to (freq, x)
-- TODO: unfoldr could be faster?

-- | \(O(\log A \min(|A|, L))\) Internal implementation of @assocs@.
{-# INLINE _assocsWithRWM #-}
_assocsWithRWM :: RawWaveletMatrix -> Int -> Int -> (Int -> Int) -> [(Int, Int)]
_assocsWithRWM RawWaveletMatrix {..} l_ r_ f
  | 0 <= l_ && l_ <= r_ && r_ < lengthRWM = execState (inner (0 :: Int) (0 :: Int) l_ (r_ + 1)) []
  | otherwise = []
  where
    -- DFS. [l, r)
    inner !acc iRow !l !r
      | iRow >= heightRWM = do
          let !n = r - l
          let !acc' = f acc
          modify' ((acc', n) :)
      | otherwise = do
          let !bits = bitsRWM G.! iRow
              !l0 = freq0BV bits l
              !r0 = freq0BV bits r
              !nZeros = nZerosRWM G.! iRow
          -- go right (visit bigger values first)
          let !l' = l + nZeros - l0
          let !r' = r + nZeros - r0
          when (l' < r') $ do
            inner (acc .|. bit (heightRWM - 1 - iRow)) (iRow + 1) l' r'
          -- go left
          when (l0 < r0) $ do
            inner acc (iRow + 1) l0 r0

-- | \(O(\log A \min(|A|, L))\) Internal implementation of @descAssoc@.
{-# INLINE _descAssocsWithRWM #-}
_descAssocsWithRWM :: RawWaveletMatrix -> Int -> Int -> (Int -> Int) -> [(Int, Int)]
_descAssocsWithRWM RawWaveletMatrix {..} l_ r_ f
  | 0 <= l_ && l_ <= r_ && r_ < lengthRWM = execState (inner (0 :: Int) (0 :: Int) l_ (r_ + 1)) []
  | otherwise = []
  where
    -- DFS. [l, r)
    inner !acc iRow !l !r
      | iRow >= heightRWM = do
          let !n = r - l
          let !acc' = f acc
          modify' ((acc', n) :)
      | otherwise = do
          let !bits = bitsRWM G.! iRow
              !l0 = freq0BV bits l
              !r0 = freq0BV bits r
              !nZeros = nZerosRWM G.! iRow
          -- go left
          when (l0 < r0) $ do
            inner acc (iRow + 1) l0 r0
          -- go right (visit bigger values first)
          let !l' = l + nZeros - l0
          let !r' = r + nZeros - r0
          when (l' < r') $ do
            inner (acc .|. bit (heightRWM - 1 - iRow)) (iRow + 1) l' r'

-- | \(O(\log A \min(|A|, L))\) Collects \((x, freq)\) in range \([l, r]\) in ascending order of
-- @x@. Be warned that it's very slow unless the domain \(A\) is very small.
{-# INLINE assocsRWM #-}
assocsRWM :: RawWaveletMatrix -> Int -> Int -> [(Int, Int)]
assocsRWM wm l_ r_ = _assocsWithRWM wm l_ r_ id

-- | \(O(\log A \min(|A|, L))\) Collects \((x, freq)\) in range \([l, r]\) in descending order of
-- @x@. Be warned that it's very slow unless the domain \(A\) is very small.
{-# INLINE descAssocsRWM #-}
descAssocsRWM :: RawWaveletMatrix -> Int -> Int -> [(Int, Int)]
descAssocsRWM wm l_ r_ = _descAssocsWithRWM wm l_ r_ id

-- TODO: topK and intersects are not implemented as they are not so fast.
