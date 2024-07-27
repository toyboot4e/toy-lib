module Tests.PowMod where

import Math.PowMod
import Test.Tasty
import Test.Tasty.QuickCheck as QC

modulo :: Int
modulo = 1009

times :: Int -> (a -> a) -> a -> a
times !n !f = inner 0
  where
    inner i !s
      | i >= n = s
      | otherwise = inner (i + 1) $! f s

powModProps :: TestTree
powModProps =
  testGroup
    "PowMod properties"
    [ QC.testProperty "powModConst" $ do
        QC.forAll (QC.chooseInt (1, modulo - 1)) $ \base -> do
          QC.forAll (QC.chooseInt (1, 100)) $ \p -> do
           times p (mulMod modulo base) 1 QC.=== powModConst modulo base p,
      QC.testProperty "invModConst" $ do
        QC.forAll (QC.chooseInt (1, modulo - 1)) $ \base -> do
           times (modulo - 2) (mulMod modulo base) 1 QC.=== invModConst modulo base,
      QC.testProperty "divModConst" $ do
        QC.forAll (QC.chooseInt (1, modulo - 1)) $ \x -> do
          QC.forAll (QC.chooseInt (1, modulo - 1)) $ \d -> do
            let expected = mulMod modulo x (times (modulo - 2) (mulMod modulo d) 1)
            let res = divModConst modulo x d
            res QC.=== expected
    ]

tests :: [TestTree]
tests = [testGroup "Math.PowMod" [powModProps]]
