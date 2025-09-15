module Tests.Bisect where

import Algorithm.Bisect
import Data.List qualified as L
import Data.Vector.Generic qualified as G
import Data.Vector.Unboxed qualified as U
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

doubleBisectProps :: TestTree
doubleBisectProps =
  testGroup
    "Double bisect properties"
    [ testCase "bisect (Double) accuracy" $ do
        let eps = 10.0 ** (-12.0) :: Double
        let (Just l, Just r) = bisectF64 eps 0.0 1.0 (<= 0.5)
        assertBool "bisect left EPS" ((0.5 - l) < eps)
        assertBool "bisect right EPS" ((r - 0.5) < eps),
      testCase "bisect (Double) boundaries" $ do
        let eps = 10.0 ** (-12.0) :: Double
        bisectF64 eps 0.0 1.0 (const True) @?= (Just 1.0, Nothing)
        bisectF64 eps 0.0 1.0 (const False) @?= (Nothing, Just 0.0)
    ]

tests :: [TestTree]
tests = [testGroup "Algorithm.Bisect" [doubleBisectProps]]
