{-# LANGUAGE LambdaCase #-}

module Tests.SumMinMax where

import Control.Monad
import Control.Monad.ST
import Data.List (foldl')
import Data.SegmentTree.Beats
import Data.SegmentTree.Beats.SumMinMax
import Data.SegmentTree.Util
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

testSMM :: TestTree
testSMM =
  testGroup
    "SumMinMax tests"
    [ testCase "SumMinMax identity" $ do
        let smm = singletonSMM (10 :: Int)
        smm <> mempty @?= smm
        mempty <> smm @?= smm,
      testCase "SumMinMax length 1" $ do
        let smm = singletonSMM (10 :: Int)
        let s1 = sactWithLength (newAddACC (15 :: Int)) smm 1
        let s2 = sactWithLength (newChminACC (5 :: Int)) smm 1
        let s3 = sactWithLength (newChminACC (15 :: Int)) smm 1
        let s4 = sactWithLength (newChmaxACC (15 :: Int)) smm 1
        let s5 = sactWithLength (newChmaxACC (5 :: Int)) smm 1
        sumSMM s1 @?= 25
        sumSMM s2 @?= 5
        sumSMM s3 @?= 10
        sumSMM s4 @?= 15
        sumSMM s5 @?= 10
    ]

testACC :: TestTree
testACC =
  testGroup
    "AddChminChmax tests"
    [ testCase "AddChminChmax identity" $ do
        let x = 10 :: Int
        newAddACC x <> mempty @?= newAddACC x
        mempty <> newAddACC x @?= newAddACC x
        newChminACC x <> mempty @?= newChminACC x
        mempty <> newChminACC x @?= newChminACC x
        newChmaxACC x <> mempty @?= newChmaxACC x
        mempty <> newChmaxACC x @?= newChmaxACC x,
      testCase "AddChminChmax simple sact" $ do
        let y = sactWithLength (newAddACC (-1 :: Int)) (singletonSMM (0 :: Int)) 1
        failsSMM y @?= False
        sumSMM y @?= -1
    ]

testCompositeACC :: TestTree
testCompositeACC =
  testGroup
    "AddChminChmax composite tests"
    [ QC.testProperty "AddChminChmax (op <> op) `sact` a = op `sact` op `sact` a" $ do
        (!a1, !a2) <- fmap ((,) <$> head <*> (!! 1)) $ QC.vectorOf 2 $ do
          t <- QC.chooseInt (0, 2)
          x <- QC.chooseInt (-10, 10)
          return $ case t of
            0 -> newChminACC x
            1 -> newChmaxACC x
            2 -> newAddACC x
            _ -> error "unreachable"
        x <- singletonSMM <$> QC.chooseInt (-10, 10)
        let len = 1 -- len <- QC.chooseInt (1, 5)
        return . QC.counterexample (show (a1, a2)) $
          sactWithLength (a1 <> a2) x len QC.=== sactWithLength a1 (sactWithLength a2 x len) len,
      QC.testProperty "AddChminChmax multi composite" $ do
        n <- QC.chooseInt (2, 10)
        as <- QC.vectorOf n $ do
          t <- QC.chooseInt (0, 2)
          x <- QC.chooseInt (-10, 10)
          return $ case t of
            0 -> newChminACC x
            1 -> newChmaxACC x
            2 -> newAddACC x
            _ -> error "unreachable"
        x <- singletonSMM <$> QC.chooseInt (-10, 10)
        let x1 = sactWithLength (foldl' (<>) mempty (reverse as)) x 1
        let x2 = foldl' (\acc op -> sactWithLength op acc 1) x as
        return . QC.counterexample (show as) $ x1 QC.=== x2
    ]

queryGen :: Int -> (Int, Int) -> Gen (Int, Int, Int, Int)
queryGen n rng = do
  -- 0: chmin, 1: chmax, 2: add, 3: answer
  t <- QC.chooseInt (0, 3)
  l <- QC.chooseInt (0, n - 1)
  r <- QC.chooseInt (l, n - 1)
  x <- QC.chooseInt rng
  return (t, l, r, x)

testBeats :: TestTree
testBeats =
  testGroup
    "Beats"
    [ QC.testProperty "Beats random test" $ do
        n <- QC.chooseInt (1, 10)
        q <- QC.chooseInt (1, 20)
        qs <- U.fromList <$> QC.vectorOf q (queryGen n (-5, 5))

        -- [(1,0,1,1),(0,0,1,-2),(3,0,0,3)]
        let res = runST $ do
              stree <- buildSTB $ U.replicate n (singletonSMM (0 :: Int))
              vec <- UM.replicate n (0 :: Int)
              (`U.mapMaybeM` qs) $ \case
                (0, !l, !r, !x) -> do
                  sactWithLengthSTB stree l r $ newChminACC x
                  forM_ [l .. r] $ UM.modify vec (min x)
                  return Nothing
                (1, !l, !r, !x) -> do
                  sactWithLengthSTB stree l r $ newChmaxACC x
                  forM_ [l .. r] $ UM.modify vec (max x)
                  return Nothing
                (2, !l, !r, !x) -> do
                  sactWithLengthSTB stree l r $ newAddACC x
                  forM_ [l .. r] $ UM.modify vec (+ x)
                  return Nothing
                (3, !l, !r, !_) -> do
                  s1 <- sumSMM <$> foldWithLengthSTB stree l r
                  s2 <- U.sum <$> U.unsafeFreeze (UM.slice l (r - l + 1) vec)
                  return $ Just (s1, s2)

        -- (beats, naive)
        let (!a1, !a2) = U.unzip res
        return $ QC.counterexample (show qs) $ a1 QC.=== a2
    ]

tests :: [TestTree]
tests = [testSMM, testACC, testCompositeACC, testBeats]
