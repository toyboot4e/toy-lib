module Tests.WaveletMatrix where

import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Extra
import Data.Vector.Generic qualified as G
import Data.Vector.Unboxed qualified as U
import Data.WaveletMatrix
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

fixedTest :: TestTree
fixedTest =
  testGroup
    "Wavelet Matrix fixed test"
    [ testCase "example" $ do
        -- The example in this article:
        -- https://miti-7.hatenablog.com/entry/2018/04/28/152259
        let xs = U.fromList [5 :: Int, 4, 5, 5, 2, 1, 5, 6, 1, 3, 5, 0]

        let bits =
              V.map U.fromList $
                V.fromList
                  [ [1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0],
                    [1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0],
                    [1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0]
                  ]

        let ints =
              V.map U.fromList $
                V.fromList
                  [ [1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0],
                    [1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0],
                    [1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0]
                  ]

        let wm = newWM 6 xs
        bits @?= bitsWM wm

        let nZeros = U.convert $ V.map ((U.length xs -) . U.sum) ints
        nZeros @?= nZerosWM wm

        let csums = V.map (U.cons 0 . U.singleton . U.sum) ints
        csums @?= csumsWM wm

        let xs' = U.generate (U.length xs) (accessWM wm)
        xs @?= xs'
    ]

randomTests :: TestTree
randomTests =
  testGroup
    "Wavelet Matrix random tests"
    [ QC.testProperty "kth smallest" $ do
        n <- QC.chooseInt (1, 100)
        xs <- U.fromList <$> QC.vectorOf n (QC.chooseInt rng)

        let dict = U.uniq $ U.modify VAI.sort xs
        let nx = U.length dict

        l <- QC.chooseInt (0, n - 1)
        r <- QC.chooseInt (l, n - 1)

        k <- QC.chooseInt (0, r - l)
        let slice = U.take (r - l + 1) $ U.drop l xs
        let expected = U.modify VAI.sort slice G.! k

        -- TODO: automatic index compression
        let xs' = U.map (bindex dict) xs
        let wm = newWM nx xs'
        let res = dict G.! kthSmallestWM wm l r k

        return . QC.counterexample (show (xs, (l, r, k))) $ expected QC.=== res
    ]
  where
    rng = (-20, 20)

tests :: [TestTree]
tests = [testGroup "Data.WaveletMatrix" [fixedTest, randomTests]]
