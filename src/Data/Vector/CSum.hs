-- | Cumulative sum in one dimension.
module Data.Vector.CSum where

import Control.Monad.Primitive
import Data.Ix
import Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Mutable as UM
import GHC.Stack (HasCallStack)
import ToyLib.Debug
import ToyLib.Macro

-- | \(O(N)\) Immutable cumulative sum initialization. The first element is a guard.
{-# INLINE csum1D #-}
csum1D :: (Num a, U.Unbox a) => U.Vector a -> U.Vector a
csum1D = U.scanl' (+) 0

-- | \(O(1)\) Retrieves a range sum from an immutable cumulative sum.
{-# INLINE (+!) #-}
(+!) :: (HasCallStack, Num a, U.Unbox a) => U.Vector a -> (Int, Int) -> a
(+!) csum (!l, !r) = csum U.! (r + 1) - csum U.! l
  where
    _
      | debug =
          let n = U.length csum - 1
              !_ = dbgAssert (inRange (0, n - 1) l && inRange (0, n - 1) r) $ "(+!) invalid range: " ++ show (l, r)
           in ()
      | otherwise = ()

-- | \(O(N)\) Initialization of a mutable cumulative sum.
{-# INLINE newCSumU #-}
newCSumU :: (PrimMonad m, Num a, U.Unbox a) => Int -> m (UM.MVector (PrimState m) a)
newCSumU n = UM.replicate (n + 1) 0

-- | \(O(1)\) The cumulative sum vector has a guard at index zero while @l@ and @r@ are zero-based.
{-# INLINE readCSum #-}
readCSum :: (PrimMonad m, Num a, GM.MVector v a) => v (PrimState m) a -> Int -> Int -> m a
readCSum vec l r = (-) <$> GM.read vec (r + 1) <*> GM.read vec l

-- | \(O(1)\) The cumulative sum vector has @len@ + 1 element (guard).
{-# INLINE appendCSum #-}
appendCSum :: (PrimMonad m, Num a, GM.MVector v a) => v (PrimState m) a -> Int -> a -> m ()
appendCSum vec len dx = do
  x <- GM.read vec len
  GM.write vec (len + 1) $! x + dx
