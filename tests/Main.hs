module Main where

import Test.Tasty
import Tests.Bisect qualified
import Tests.PowMod qualified
import Tests.Slide qualified
import Tests.SlideMin qualified
import Tests.SumMinMax qualified

tests :: [TestTree]
tests =
  concat
    [ Tests.Bisect.tests,
      Tests.PowMod.tests,
      Tests.Slide.tests,
      Tests.SlideMin.tests,
      Tests.SumMinMax.tests
    ]

main :: IO ()
main = defaultMain $ testGroup "topLevel" tests

-- -- | QuickCheck options: ishow generated values on failure.
-- qcArgs :: QC.Args
-- qcArgs = QC.stdArgs {QC.chatty = True}

-- main = defaultMainWithIngredients (includingOptions [QC.QuickCheck qcArgs]) $ testGroup "topLevel" tests
