module Main where

import Test.Tasty
import Tests.Bisect qualified
import Tests.NTT qualified
import Tests.RangeMap qualified

main :: IO ()
main =
  defaultMain $
    testGroup "topLevel" $
      Tests.Bisect.tests ++ Tests.NTT.tests ++ Tests.RangeMap.tests
