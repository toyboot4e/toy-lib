module Main where

import Test.Tasty
import Tests.Bisect qualified
import Tests.IntervalMap qualified
import Tests.NTT qualified
import Tests.PUnionFind qualified
import Tests.PowMod qualified
import Tests.SplayMap qualified
import Tests.SplaySeq qualified
import Tests.SuffixArray qualified
import Tests.SumMinMax qualified
import Tests.WaveletMatrix qualified
import Tests.ZFunction qualified

tests :: [TestTree]
tests =
  concat
    [ Tests.Bisect.tests,
      Tests.NTT.tests,
      Tests.IntervalMap.tests,
      Tests.PowMod.tests,
      Tests.PUnionFind.tests,
      Tests.SplayMap.tests,
      Tests.SplaySeq.tests,
      Tests.SuffixArray.tests,
      Tests.SumMinMax.tests,
      Tests.WaveletMatrix.tests,
      Tests.ZFunction.tests
    ]

main :: IO ()
main = defaultMain $ testGroup "topLevel" tests

-- -- | QuickCheck options: ishow generated values on failure.
-- qcArgs :: QC.Args
-- qcArgs = QC.stdArgs {QC.chatty = True}

-- main = defaultMainWithIngredients (includingOptions [QC.QuickCheck qcArgs]) $ testGroup "topLevel" tests
