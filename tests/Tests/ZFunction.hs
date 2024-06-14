-- | ZFunction tests.
module Tests.ZFunction where

import Data.ByteString.Char8 qualified as BS
import Data.ByteString.ZFunction
import Test.Tasty
import Test.Tasty.QuickCheck as QC

genBS :: Gen BS.ByteString
genBS = do
  len <- choose (1, 100)
  BS.pack <$> vectorOf len (QC.oneof [QC.choose ('a', 'z'), QC.choose ('A', 'Z')])

zFunctionProps :: TestTree
zFunctionProps =
  testGroup
    "Z function properties"
    [ QC.testProperty "naiveZ == fastZ" $ do
        s <- genBS
        return
          . QC.counterexample (show s)
          $ zOfNaive s QC.=== zOf s
    ]

-- FIXME: sometimes fails?
tests :: [TestTree]
tests = [testGroup "Data.ByteString.ZFunction" [zFunctionProps]]
