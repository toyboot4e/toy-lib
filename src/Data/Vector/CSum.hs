-- | Cummulative sum in one dimension.
module Data.Vector.CSum where

import Control.Monad.Primitive
import Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Mutable as UM

{-# INLINE csum1D #-}
csum1D :: (Num a, U.Unbox a) => U.Vector a -> U.Vector a
csum1D = U.scanl' (+) 0

{-# INLINE (+!) #-}
(+!) :: (Num a, U.Unbox a) => U.Vector a -> (Int, Int) -> a
(+!) csum (!l, !r) = csum U.! (r + 1) - csum U.! l

{-# INLINE newCSumU #-}
newCSumU :: (PrimMonad m, Num a, U.Unbox a) => Int -> m (UM.MVector (PrimState m) a)
newCSumU n = UM.replicate (n + 1) 0

-- | The cumulative sum vector has a guard at index zero while @l@ and @r@ are zero-based.
{-# INLINE readCSum #-}
readCSum :: (PrimMonad m, Num a, GM.MVector v a) => v (PrimState m) a -> Int -> Int -> m a
readCSum vec l r = (-) <$> GM.read vec (r + 1) <*> GM.read vec l

-- | The cumulative sum vector has @len@ + 1 element (guard).
{-# INLINE snocCSum #-}
snocCSum :: (PrimMonad m, Num a, GM.MVector v a) => v (PrimState m) a -> Int -> a -> m ()
snocCSum vec len dx = do
  x <- GM.read vec len
  GM.write vec (len + 1) $! x + dx
