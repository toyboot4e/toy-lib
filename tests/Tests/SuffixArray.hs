-- | SuffixArray tests.
module Tests.SuffixArray where

import Data.ByteString.Char8 qualified as BS
import Data.ByteString.SuffixArray
import Test.Tasty
import Test.Tasty.QuickCheck as QC

genBS :: Gen BS.ByteString
genBS = do
  -- len <- choose (1, 100)
  len <- choose (1, 5)
  BS.pack <$> vectorOf len (QC.oneof [QC.choose ('a', 'z'), QC.choose ('A', 'Z')])

suffixArrayProps :: TestTree
suffixArrayProps =
  testGroup
    "Suffix Array properties"
    [ QC.testProperty "naiveSA == fastSA" $ do
        s <- genBS
        return
          . QC.counterexample (show (BS.unpack s, saOfNaive s, saOf s))
          $ saOfNaive s QC.=== saOf s
    ]

tests :: [TestTree]
tests = [testGroup "Data.ByteString.SuffixArray" [suffixArrayProps]]
