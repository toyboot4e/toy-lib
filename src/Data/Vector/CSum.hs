-- | Cummulative sum in one dimension.
module Data.Vector.CSum where

import qualified Data.Vector.Unboxed as U

{-# INLINE csum1D #-}
csum1D :: (Num a, U.Unbox a) => U.Vector a -> U.Vector a
csum1D = U.scanl' (+) 0

{-# INLINE (+!) #-}
(+!) :: (Num a, U.Unbox a) => U.Vector a -> (Int, Int) -> a
(+!) csum (!l, !r) = csum U.! succ r - csum U.! l
