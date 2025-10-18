module Tests.SlideMin where

import Algorithm.SlideMin
import AtCoder.SegTree qualified as Seg
import Control.Monad.ST (runST)
import Data.Semigroup
import Data.Vector.Unboxed qualified as U
import Test.Tasty
import Test.Tasty.QuickCheck as QC

props :: TestTree
props =
  testGroup
    "SlideMin random test"
    [ QC.testProperty "slide-stack" $ do
        n <- QC.chooseInt (1, 16)
        len <- QC.chooseInt (1, n)
        xs <- U.fromList <$> QC.vectorOf n (QC.chooseInt (-16, 16))
        let expected = runST $ do
              stree <- Seg.build $ U.map Min xs
              U.generateM (n - (len - 1)) $ \i -> do
                Min x <- Seg.prod stree i (i + len)
                return x
        let result = U.backpermute xs $ slideMinIndices len xs
        return $ result QC.=== expected
    ]

tests :: [TestTree]
tests = [testGroup "Algorithm.SlideMin" [props]]
