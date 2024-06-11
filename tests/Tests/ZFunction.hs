-- | ZFunction tests.

module Tests.ZFunction where

import qualified Data.ByteString.Char8 as BS
import Data.ZFunction
import Test.Tasty
import Test.Tasty.QuickCheck as QC

genBS :: Gen BS.ByteString
genBS = do
    len <- choose (1, 100)
    BS.pack <$> vectorOf len arbitrary

zFunctionProps :: TestTree
zFunctionProps =
  testGroup
    "Z function properties"
    [ QC.testProperty "naiveZ == fastZ" $ do
        s <- genBS
        return $ zOfNaive s QC.=== zOf s
    ]

tests :: [TestTree]
tests = [testGroup "Data.ZFunction" [zFunctionProps]]

