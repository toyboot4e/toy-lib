{-# LANGUAGE DataKinds #-}

-- | WIP Number-theoretic transform (integer DFT).
module Math.NTT where

import Control.Monad (forM_)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bits
import Data.ModInt
import Data.Ord (comparing)
import qualified Data.Vector.Algorithms.Intro as VAI
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.TypeLits
import ToyLib.Debug (dbgAssert)

-- import Debug.Trace

-- | ntt. TODO: re-sorting the output?
--
-- >>> :set -XDataKinds
-- >>> ntt $ U.fromList $ map (ModInt @998244353) [1, 1, 1, 1]
-- [4,0,0,0]
-- >>> ntt $ U.fromList $ map (ModInt @469762049) [123, 0, 0, 0]
-- [123,123,123,123]
ntt :: (KnownNat p) => U.Vector (ModInt p) -> U.Vector (ModInt p)
ntt xs =
  U.modify butterfly $
    U.map snd $
      -- TODO: consider removing sort
      U.modify (VAI.sortBy (comparing (bitReverse . fst))) $
        U.indexed $
          grow2 xs

-- | \(O(N \log N)\)
butterfly :: forall p m. (KnownNat p, PrimMonad m) => UM.MVector (PrimState m) (ModInt p) -> m ()
butterfly xs = do
  let !inv2 = ModInt @p 1 / ModInt @p 2
  (\f -> U.foldM'_ f (ModInt @p 1) (U.enumFromN 1 iMaxBit)) $ \rot1 iBit -> do
    butterfly1 xs rot1 iMaxBit iBit
    return $! rot1 * inv2
  where
    -- TODO: validate the number of digits
    !_ = dbgAssert (popCount (UM.length xs) == 1) "not a power of two"
    !iMaxBit = countTrailingZeros $ UM.length xs

-- | \(O(N)\) Butterfly calculation performed in-place.
--
-- - @iMaxBit@: Length of @xs@ equals to @bit iMaxBit@
-- - @iBit@: The length of a pair equals to the length of @xs@.
butterfly1 :: forall p m. (KnownNat p, PrimMonad m) => UM.MVector (PrimState m) (ModInt p) -> ModInt p -> Int -> Int -> m ()
butterfly1 xs rot1 iMaxBit iBit = do
  let !nBoxes = bit (iMaxBit - iBit)
  -- interval between pairs
  let !interval = bit (iBit - 1)
  forM_ [0 .. nBoxes - 1] $ \iBox -> do
    (\f -> U.foldM'_ f (1 :: ModInt p) (U.generate (bit (iBit - 1)) id)) $ \rot iPair -> do
      let !i = iBox * bit iBit + iPair
      -- let !_ = traceShow (iMaxBit, i, rot, iBox, iPair) ()
      !x1 <- UM.read xs i
      !x2 <- UM.read xs (i + interval)
      UM.write xs i $! x1 + x2 * rot
      UM.write xs (i + interval) $! x1 - x2 * rot
      return $ rot * rot1
  where
    !_ = dbgAssert (popCount (UM.length xs) == 1) "not a power of two"

-- | https://www.linkedin.com/pulse/%E7%B5%B6%E5%AF%BE%E3%81%AB%E3%82%84%E3%81%A3%E3%81%A6%E3%81%AF%E3%81%84%E3%81%91%E3%81%AA%E3%81%84%E3%83%93%E3%83%83%E3%83%88%E5%8F%8D%E8%BB%A2-masayuki-tatebe?articleId=6539466321338425345
--
-- TODO: Is it really working as expected??
bitReverse :: Int -> Int
bitReverse x0 =
  let !x1 = (x0 .&. 0x55555555) .<<. 1 .|. (x0 .>>. 1) .&. 0x55555555
      !x2 = (x1 .&. 0x33333333) .<<. 2 .|. (x1 .>>. 2) .&. 0x33333333
      !x3 = (x2 .&. 0x0F0F0F0F) .<<. 4 .|. (x2 .>>. 4) .&. 0x0F0F0F0F
      !x4 = (x3 .<<. 24) .|. ((x3 .&. 0xFF00) .<<. 8) .|. ((x3 .>>. 8) .&. 0xFF00) .|. (x3 .>>. 24)
   in x4

-- | Grow to the length of a power two. FIXME: faster.
grow2 :: forall a. (Num a, U.Unbox a) => U.Vector a -> U.Vector a
grow2 xs
  | U.length xs < bit n = xs U.++ U.replicate (bit n - U.length xs) (0 :: a)
  | otherwise = xs
  where
    n = until (\i -> bit i >= U.length xs) (.<<. 1) (1 :: Int)
