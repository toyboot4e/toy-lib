module Tests.WaveletMatrix where

import Control.Monad.ST (runST)
import Data.WaveletMatrix
import Data.List qualified as L
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

wmProps :: TestTree
wmProps =
  testGroup
    "Wavelet Matrix properties"
    [ testCase "example" $ do
        -- The example in this article:
        -- https://miti-7.hatenablog.com/entry/2018/04/28/152259
        let xs = U.fromList [5 :: Int, 4, 5, 5, 2, 1, 5, 6, 1, 3, 5, 0]
        let vec = V.map U.fromList $ V.fromList
             [ [1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0],
               [1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0],
               [1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0]
             ]
        let vec' = runST $ do
              wm <- newWM 6 xs
              V.mapM U.unsafeFreeze (bitsWM wm)
        vec @?= vec'
    ]

tests :: [TestTree]
tests = [testGroup "Data.WaveletMatrix" [wmProps]]

