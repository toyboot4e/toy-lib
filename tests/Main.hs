module Main where

import Test.Tasty
import qualified Tests.Bisect

main :: IO ()
main = defaultMain Tests.Bisect.tests
