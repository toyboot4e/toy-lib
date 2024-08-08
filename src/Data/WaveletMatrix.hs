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

-- | Wavelet matrix.
data WaveletMatrix = WaveletMatrix
  { -- \(\lceil \log N \rceil\).
    heightWM :: {-# UNPACK #-} !Int,
    -- The bit matrix. Each row represents (heightWM - 1 - iRow) bit's on/off.
    bitsWM :: !(V.Vector (U.Vector Bit)),
    -- The number of zeros in each row in the bit matrix.
    nZerosWM :: !(U.Vector Int),
    -- The cummulative sum of each row in the bit matrix.
    csumsWM :: !(V.Vector (U.Vector Int))
  }
  deriving (Eq, Show)

-- | Creates a `WaveletMatrix`.
--
-- - @nx@: The number of different @x@.
{-# INLINE newWM #-}
newWM :: Int -> U.Vector Int -> WaveletMatrix
newWM nx xs = runST $ do
  orgBits <- UM.replicate (n * heightWM) $ Bit False
  orgCsum <- UM.replicate ((n + 1) * heightWM) 0
  let !bits = toMatUM n orgBits
  let !csums = toMatUM (n + 1) orgCsum
  nZeros <- UM.unsafeNew heightWM

  vec <- U.thaw xs
  V.iforM_ (V.zip bits csums) $ \iRow (!bits, !csum) -> do
    let !iBit = heightWM - 1 - iRow
    vec' <- U.unsafeFreeze vec
    -- U.imapM_ vec' -- TODO: which is faster, imapM_ vs unsafeWrite?
    U.iforM_ vec' $ \i x -> do
      GM.unsafeWrite bits i . Bit $ testBit x iBit

    -- csum
    GM.unsafeWrite csum 0 (0 :: Int)
    bits' <- U.unsafeFreeze bits
    nOnes <-
      U.ifoldM'
        ( \ !acc i (Bit !b) -> do
            let !acc' = acc + (fromEnum b :: Int)
            GM.unsafeWrite csum (i + 1) acc'
            return acc'
        )
        (0 :: Int)
        bits'

    UM.write nZeros iRow (n - nOnes)

    -- Run stable sort over xs. TODO: Can it be faster if I don't use VAR?
    VAR.sortBy 2 2 (\_ x -> fromEnum (testBit x iBit)) vec

  -- FIXME: Is it SAFE to use unsafeFreeze over dropped vector?
  bitsWM <- toMatU n <$> U.unsafeFreeze orgBits
  nZerosWM <- U.unsafeFreeze nZeros
  csumsWM <- toMatU (n + 1) <$> U.unsafeFreeze orgCsum
  return $ WaveletMatrix {..}
  where
    !n = G.length xs
    -- TODO: use bit operations
    (!heightWM, !_) = until ((>= nx) . snd) (bimap succ (* 2)) (0 :: Int, 1 :: Int)
    toMatUM m = V.unfoldrExactN heightWM (UM.splitAt m)
    toMatU m = V.unfoldrExactN heightWM (U.splitAt m)

-- | Counts the number of 0 bits in interval [0, i].
{-# INLINE rank0WM #-}
rank0WM :: WaveletMatrix -> Int -> Int -> Int
rank0WM wm iRow i = i + 1 - rank1WM wm iRow i

-- | Counts the number of 1 bits in interval [0, i].
{-# INLINE rank1WM #-}
rank1WM :: WaveletMatrix -> Int -> Int -> Int
rank1WM wm iRow i = popCount $ G.take (i + 1) (bitsWM wm G.! iRow)

-- | Counts the number of 0 bits in interval [0, i).
{-# INLINE rank0BV #-}
rank0BV :: U.Vector Bit -> Int -> Int
rank0BV bits i = i - rank1BV bits i

-- | Counts the number of 1 bits in interval [0, i).
{-# INLINE rank1BV #-}
rank1BV :: U.Vector Bit -> Int -> Int
rank1BV bits i = popCount $ G.take i bits

-- | \(O(\log a)\) Returns @a[k]@.
accessWM :: WaveletMatrix -> Int -> Int
accessWM WaveletMatrix {..} i0 = res
  where
    (!_, !res) =
      V.ifoldl'
        ( \(!i, !acc) iRow !bits ->
            let Bit !b = G.unsafeIndex bits i
                !i' = if b then rank1BV bits i + (nZerosWM G.! iRow) else rank0BV bits i
                !acc' = if b then setBit acc (heightWM - 1 - iRow) else acc
             in (i', acc')
        )
        (i0, 0)
        -- TODO: indexing can be faster
        bitsWM
