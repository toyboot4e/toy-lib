-- | Test utilities
module Tests.Util where

import Data.Vector.Unboxed qualified as U
import Test.Tasty.QuickCheck as QC

rangeGen :: Int -> Gen (Int, Int)
rangeGen n = do
  l <- QC.chooseInt (0, n - 1)
  r <- QC.chooseInt (l, n - 1)
  return (l, r)


sliceLR :: (U.Unbox a) => Int -> Int -> U.Vector a -> U.Vector a
sliceLR l r = U.take (r - l + 1) . U.drop l

