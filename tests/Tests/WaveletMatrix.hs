module Tests.WaveletMatrix where
import Data.WaveletMatrix
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U
import Test.Tasty
import Test.Tasty.HUnit

wmProps :: TestTree
wmProps =
  testGroup
    "Wavelet Matrix properties"
    [ testCase "example" $ do
        -- The example in this article:
        -- https://miti-7.hatenablog.com/entry/2018/04/28/152259
        let xs = U.fromList [5 :: Int, 4, 5, 5, 2, 1, 5, 6, 1, 3, 5, 0]

        let bits = V.map U.fromList $ V.fromList
             [ [1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0],
               [1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0],
               [1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0]
             ]

        let ints = V.map U.fromList $ V.fromList
             [ [1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0],
               [1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0],
               [1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0]
             ]

        let wm = newWM 6 xs
        bits @?= bitsWM wm

        let nZeros = U.convert $ V.map ((U.length xs -) . U.sum) ints
        nZeros @?= nZerosWM wm

        let xs' = U.generate (U.length xs) (accessWM wm)
        xs @?= xs'
    ]

tests :: [TestTree]
tests = [testGroup "Data.WaveletMatrix" [wmProps]]

