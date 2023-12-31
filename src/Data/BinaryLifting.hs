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
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import GHC.Stack (HasCallStack)
import ToyLib.Macro (dbgAssert)
import ToyLib.Prelude (rangeG)

-- | Storage of \(s^{2^i}\).
newtype BinaryLifting v m = BinaryLifting (v m)
  deriving (Show, Eq)

-- | Calculates `BinaryLifting` of the given semigroup
newBinLift :: (G.Vector v s, Semigroup s) => s -> BinaryLifting v s
newBinLift !op0 = BinaryLifting ops
  where
    !ops = G.iterateN (pred 63) (\ !op -> op <> op) op0

-- | Calculates `BinaryLifting` of the given semigroup
newBinLiftV :: (Semigroup s) => s -> BinaryLifting V.Vector s
newBinLiftV = newBinLift

-- | Calculates `BinaryLifting` of the given semigroup
newBinLiftU :: (Semigroup s, U.Unbox s) => s -> BinaryLifting U.Vector s
newBinLiftU = newBinLift

-- | Binarily lifted version of `stimesMonoid`.
-- WARNING: Usually `sactBL` is much cheaper for semigroup actions with a boxed type.
stimesBL :: (HasCallStack, Semigroup s, G.Vector v s) => BinaryLifting v s -> s -> Int -> s
stimesBL (BinaryLifting !ops) !s0 !n = U.foldl' step s0 (U.enumFromN 0 62)
  where
    step !m !i
      | testBit n i = m <> ops G.! i
      | otherwise = m

-- | Binarily lifted version of `stimesMonoid`.
-- WARNING: Usually `sactBL` is much cheaper for semigroup actions with a boxed type.
mtimesBL :: (HasCallStack, Monoid m, G.Vector v m) => BinaryLifting v m -> Int -> m
mtimesBL !bin !n = stimesBL bin mempty n

-- | Binarily lifted semigroup action application.
sactBL :: (HasCallStack, SemigroupAction s a, G.Vector v s) => BinaryLifting v s -> a -> Int -> a
sactBL (BinaryLifting !ops) !acc0 !nAct = U.foldl' step acc0 (rangeG 0 62)
  where
    step !acc !nBit
      | testBit nAct nBit = (ops G.! nBit) `sact` acc
      | otherwise = acc

-- | Alias of `sactBL` for monoid action.
mactBL :: (HasCallStack, MonoidAction m a, G.Vector v m) => BinaryLifting v m -> a -> Int -> a
mactBL = sactBL

-- | Old binary lifting implementation without typeclasses. TODO: Remove
newDoubling :: (G.Vector v a, G.Vector v Int) => a -> (a -> a) -> v a
newDoubling !oper0 !squareCompositeF = G.scanl' step oper0 $! G.enumFromN (1 :: Int) 62
  where
    step !oper !_ = squareCompositeF oper

-- | Old binary lifting implementation without typeclasses. TODO: Remove
newDoublingV :: a -> (a -> a) -> V.Vector a
newDoublingV = newDoubling

-- | Old binary lifting implementation without typeclasses. TODO: Remove
applyDoubling :: (HasCallStack, G.Vector v op) => v op -> a -> (a -> op -> a) -> Int -> a
applyDoubling !opers !x0 !act !n = foldl' step x0 [0 .. 62]
  where
    !_ = dbgAssert $ G.length opers == 63
    step !acc !nBit =
      if testBit n nBit
        then acc `act` (opers G.! nBit)
        else acc
