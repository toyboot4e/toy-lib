{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.BinaryLifting where

import Data.Bits
import Data.List
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import ToyLib.Macro (dbgAssert)

-- {{{ Binary lifting data structure

-- | Binary lifting is a technique for calculating nth power of monoid in a (big) constant time.
--
-- The i-th element of the underlying vector of `BinaryLifting` stores `m^{2^i}`, with which we
-- can construct any of `m^i` (`0 <= i < 2^63`).
newtype BinaryLifting v m = BinaryLifting (v m)
  deriving (Show, Eq)

-- | Calculates `BinaryLifting` of the given semigroup
newBinLift :: (VG.Vector v s, Semigroup s) => s -> BinaryLifting v s
newBinLift !op0 = BinaryLifting ops
  where
    !ops = VG.iterateN (pred 63) (\op -> op <> op) op0

-- | Calculates `BinaryLifting` of the given semigroup
newBinLiftV :: Semigroup s => s -> BinaryLifting V.Vector s
newBinLiftV = newBinLift

-- | Calculates `BinaryLifting` of the given semigroup
newBinLiftVU :: (Semigroup s, VU.Unbox s) => s -> BinaryLifting VU.Vector s
newBinLiftVU = newBinLift

-- | Binarily lifted version of `stimesMonoid`.
-- WARNING: Usually `sactBL` is much cheaper for semigroup actions with a boxed type.
stimesBL :: (Semigroup s, VG.Vector v s) => (BinaryLifting v s) -> s -> Int -> s
stimesBL (BinaryLifting !ops) !s0 !n = VU.foldl' step s0 (VU.enumFromN 0 62)
  where
    step !m !i
      | testBit n i = m <> ops VG.! i
      | otherwise = m

-- | Binarily lifted version of `stimesMonoid`.
-- WARNING: Usually `sactBL` is much cheaper for semigroup actions with a boxed type.
mtimesBL :: (Monoid m, VG.Vector v m) => (BinaryLifting v m) -> Int -> m
mtimesBL !bin !n = stimesBL bin mempty n

-- }}}

-- {{{ Doubling (old)

-- | Extends an operator monoid to be able to be applied multiple times in a constant time (N < 2^62).
newDoubling :: (VG.Vector v a, VG.Vector v Int) => a -> (a -> a) -> v a
newDoubling !oper0 !squareCompositeF = VG.scanl' step oper0 $ VG.enumFromN (1 :: Int) 62
  where
    step !oper !_ = squareCompositeF oper

newDoublingV :: a -> (a -> a) -> V.Vector a
newDoublingV = newDoubling

-- | Applies an operator `n` times using the action function.
applyDoubling :: (VG.Vector v op) => v op -> a -> (a -> op -> a) -> Int -> a
applyDoubling !opers !x0 !act !n = foldl' step x0 [0 .. 62]
  where
    !_ = dbgAssert $ VG.length opers == 63
    step !acc !nBit =
      if testBit n nBit
        then acc `act` (opers VG.! nBit)
        else acc

-- }}}
