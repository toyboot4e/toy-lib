module Tests.SlideMin where

import Algorithm.SlideMin
import Control.Monad.ST (runST)
import Data.SegmentTree.Strict
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
              stree <- buildSTree $ U.map Min xs
              U.generateM (n - (len - 1)) $ \i -> do
                Min x <- foldSTree stree i (i + len - 1)
                return x
        let result = U.backpermute xs $ slideMinIndices len xs
        return $ result QC.=== expected
    ]

tests :: [TestTree]
tests = [testGroup "Algorithm.SlideMin" [props]]
