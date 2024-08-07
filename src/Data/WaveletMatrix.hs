{-# LANGUAGE RecordWildCards #-}

-- | Wavelet Matrix.
module Data.WaveletMatrix where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bit
import Data.Bifunctor (bimap)
import Data.Bits
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Algorithms.Radix as VAR

-- | Wavelet matrix.
data WaveletMatrix s = WaveletMatrix
  { -- \(\lceil \log N \rceil\).
    heightWM :: {-# UNPACK #-} !Int,
    -- The bit matrix. Each row represents (heightWM - 1 - iRow) bit's on/off.
    bitsWM :: !(V.Vector (UM.MVector s Bit)),
    -- The number of zeros in each row in the bit matrix.
    nZerosWM :: !(U.Vector Int),
    -- The cummulative sum of each row in the bit matrix.
    csumsWM :: !(V.Vector (UM.MVector s Int))
  }

-- | Frozen Wavelet matrix. TODO: Needed?
data FrozenWaveletMatrix = FrozenWaveletMatrix
  { -- \(\lceil \log N \rceil\).
    heightFWM :: {-# UNPACK #-} !Int,
    -- The bit matrix. Each row represents (heightWM - 1 - iRow) bit's on/off.
    bitsFWM :: !(V.Vector (U.Vector Bit)),
    -- The number of zeros in each row in the bit matrix.
    nZerosFWM :: !(U.Vector Int),
    -- The cummulative sum of each row in the bit matrix.
    csumsFWM :: !(V.Vector (U.Vector Int))
  } deriving (Eq, Show)

-- | Creates a `WaveletMatrix`.
--
-- - @nx@: The number of different @x@.
{-# INLINE newWM #-}
newWM :: forall m. (PrimMonad m) => Int -> U.Vector Int -> m (WaveletMatrix (PrimState m))
newWM nx xs = do
  bitsWM <- toMat n <$> UM.unsafeNew (n * heightWM)
  csumsWM <- toMat (n + 1) <$> UM.unsafeNew ((n + 1) * heightWM)
  nZeros <- UM.unsafeNew heightWM

  vec <- U.thaw xs
  V.iforM_ (V.zip bitsWM csumsWM) $ \iRow (!bits, !csum) -> do
    let !iBit = heightWM - 1 - iRow
    vec' <- U.unsafeFreeze vec
    -- U.imapM_ vec' -- TODO: which is faster, imapM_ vs unsafeWrite?
    U.iforM_ vec' $ \i x -> do
      GM.unsafeWrite bits i . Bit $ testBit x iBit

    -- csum
    GM.unsafeWrite csum 0 (0 :: Int)
    bits' <- U.unsafeFreeze bits
    nOnes <- U.ifoldM'
      ( \ !acc i (Bit b) -> do
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
  !nZerosWM <- U.unsafeFreeze nZeros
  return $ WaveletMatrix {..}
  where
    !n = G.length xs
    -- TODO: use bit operations
    (!heightWM, !_) = until ((>= nx) . snd) (bimap succ (* 2)) (0 :: Int, 1 :: Int)
    toMat m = V.unfoldrExactN heightWM (UM.splitAt m)
