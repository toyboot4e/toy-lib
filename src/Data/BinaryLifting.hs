{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Binary lifting is a technique for calculating nth power of a semigroup element in a (big)
-- constant time, or applying them to their semigroup action target.
--
-- The i-th element of the underlying vector of `BinaryLifting` stores \(s^{2^i}\), with which we
-- can construct any of \(s^i\) (\(0 <= i < 2^63\)) in a big (63) constant time.
module Data.BinaryLifting where

import Data.Bits
import Data.List
import Data.SemigroupAction
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import ToyLib.Macro (dbgAssert)
import ToyLib.Prelude (rangeVG)

-- | Storage of \(s^{2^i}\).
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

-- | Binarily lifted semigroup action application.
sactBL :: (SemigroupAction s a, VG.Vector v s) => (BinaryLifting v s) -> a -> Int -> a
sactBL (BinaryLifting !ops) !acc0 !nAct = VU.foldl' step acc0 (rangeVG 0 62)
  where
    step !acc !nBit
      | testBit nAct nBit = (ops VG.! nBit) `sact` acc
      | otherwise = acc

-- | Alias of `sactBL` for monoid action.
mactBL :: (MonoidAction m a, VG.Vector v m) => (BinaryLifting v m) -> a -> Int -> a
mactBL = sactBL

-- | Old binary lifting implementation without typeclasses. TODO: Remove
newDoubling :: (VG.Vector v a, VG.Vector v Int) => a -> (a -> a) -> v a
newDoubling !oper0 !squareCompositeF = VG.scanl' step oper0 $ VG.enumFromN (1 :: Int) 62
  where
    step !oper !_ = squareCompositeF oper

-- | Old binary lifting implementation without typeclasses. TODO: Remove
newDoublingV :: a -> (a -> a) -> V.Vector a
newDoublingV = newDoubling

-- | Old binary lifting implementation without typeclasses. TODO: Remove
applyDoubling :: (VG.Vector v op) => v op -> a -> (a -> op -> a) -> Int -> a
applyDoubling !opers !x0 !act !n = foldl' step x0 [0 .. 62]
  where
    !_ = dbgAssert $ VG.length opers == 63
    step !acc !nBit =
      if testBit n nBit
        then acc `act` (opers VG.! nBit)
        else acc
