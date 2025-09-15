-- | `IntervalMap` tests.
module Tests.IntervalMap where

-- `groupBy`, but with adjacent elements

import Data.IntMap qualified as IM
import Data.IntervalMap
import Data.List qualified as L
import Data.List.HT qualified as HT
import Test.Tasty
import Test.Tasty.QuickCheck as QC

-- Command: (isInsert, (l, r, x))

-- | Creates int map from commands.
naive :: [(Bool, (Int, Int, Int))] -> IM.IntMap Int
naive = L.foldl' step s0 . concatMap f
  where
    f (!b, (!l, !r, !x)) = [(b, (i, x)) | i <- [l .. r]]
    s0 = IM.empty
    step im (!b, (!i, !x))
      | b = IM.insert i x im
      | otherwise = IM.delete i im

foldNaive :: IM.IntMap Int -> [(Int, (Int, Int))]
foldNaive = map g . HT.groupBy f . IM.assocs
  where
    f (!i1, !x1) (!i2, !x2) = i1 + 1 == i2 && x1 == x2
    g ixs =
      let !l = fst (head ixs)
          !r = fst (last ixs)
          !x = snd (head ixs)
       in (l, (r, x))

intervalMap :: [(Bool, (Int, Int, Int))] -> IntervalMap Int
intervalMap =
  L.foldl'
    ( \rm (!b, (!l, !r, !x)) ->
        if b
          then insertIM l r x rm
          else deleteIM l r rm
    )
    emptyIM

valueSpanGen :: Int -> Int -> Int -> Int -> Gen (Bool, (Int, Int, Int))
valueSpanGen l0 r0 xl xr = do
  l <- QC.chooseInt (l0, r0)
  r <- QC.chooseInt (l, r0)
  x <- QC.chooseInt (xl, xr)
  return (True, (l, r, x))

commandGen :: Int -> Int -> Int -> Int -> Gen (Bool, (Int, Int, Int))
commandGen l0 r0 xl xr = do
  e <- QC.chooseInt (1, 5)
  l <- QC.chooseInt (l0, r0)
  r <- QC.chooseInt (l, r0)
  x <- QC.chooseInt (xl, xr)
  return (e == 1, (l, r, x))

intervalMapProps :: TestTree
intervalMapProps =
  testGroup
    "IntervalMap properties"
    [ QC.testProperty "IntervalMap: insert" $ do
        QC.forAll (QC.chooseInt (1, 100)) $ \n -> do
          QC.forAll (QC.vectorOf n (valueSpanGen (-50) 50 (-50) 50)) $ \commands -> do
            let rm1 = foldNaive $ naive commands
            let rm2 = IM.toList . unIM $ intervalMap commands
            rm1 QC.=== rm2,
      QC.testProperty "IntervalMap: insert or delete" $ do
        QC.forAll (QC.chooseInt (1, 100)) $ \n -> do
          QC.forAll (QC.vectorOf n (commandGen (-50) 50 (-50) 50)) $ \commands -> do
            let rm1 = foldNaive $ naive commands
            let rm2 = IM.toList . unIM $ intervalMap commands
            rm1 QC.=== rm2
    ]

tests :: [TestTree]
tests = [testGroup "Data.IntervalMap" [intervalMapProps]]
