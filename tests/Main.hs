module Main where

import Test.QuickCheck qualified as QC
import Test.Tasty
import Tests.Bisect qualified
import Tests.IntervalMap qualified
import Tests.NTT qualified
import Tests.SplaySMap qualified
import Tests.SuffixArray qualified
import Tests.ZFunction qualified

tests :: [TestTree]
tests =
  concat
    [ Tests.Bisect.tests,
      Tests.NTT.tests,
      Tests.IntervalMap.tests,
      Tests.SplaySMap.tests,
      Tests.SuffixArray.tests,
      Tests.ZFunction.tests
    ]

main :: IO ()
main = defaultMain $ testGroup "topLevel" tests

-- -- | QuickCheck options: ishow generated values on failure.
-- qcArgs :: QC.Args
-- qcArgs = QC.stdArgs {QC.chatty = True}

-- main = defaultMainWithIngredients (includingOptions [QC.QuickCheck qcArgs]) $ testGroup "topLevel" tests
