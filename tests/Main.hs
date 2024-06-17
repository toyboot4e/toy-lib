module Main where

import Test.Tasty
import Tests.Bisect qualified
import Tests.IntervalMap qualified
import Tests.NTT qualified
import Tests.SuffixArray qualified
import Tests.ZFunction qualified

tests :: [TestTree]
tests = concat [Tests.Bisect.tests, Tests.NTT.tests, Tests.IntervalMap.tests, Tests.ZFunction.tests, Tests.SuffixArray.tests]

main :: IO ()
main = defaultMain $ testGroup "topLevel" tests
