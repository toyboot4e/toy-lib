module Tests.SumMinMax where

import Data.SegmentTree.Beats.SumMinMax
import Data.SegmentTree.Util
import Test.Tasty
import Test.Tasty.HUnit

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
        sumSMM s5 @?= 10,
      testCase "SumMinMax length 2: 1" $ do
        let smm = singletonSMM (10 :: Int) <> singletonSMM (10 :: Int)
        minSMM smm @?= 10
        maxSMM smm @?= 10
        min2SMM smm @?= 10
        max2SMM smm @?= 10
        nMinSMM smm @?= 2
        nMaxSMM smm @?= 2
        let s1 = sactWithLength (newAddACC (15 :: Int)) smm 2
        sumSMM s1 @?= 50
        let s2 = sactWithLength (newChminACC (5 :: Int)) smm 2
        sumSMM s2 @?= 10
        let s3 = sactWithLength (newChminACC (15 :: Int)) smm 2
        sumSMM s3 @?= 20
        let s4 = sactWithLength (newChmaxACC (15 :: Int)) smm 2
        sumSMM s4 @?= 30
        let s5 = sactWithLength (newChmaxACC (5 :: Int)) smm 2
        sumSMM s5 @?= 20,
      testCase "SumMinMax length 2: 2" $ do
        let smm = singletonSMM (10 :: Int) <> singletonSMM (15 :: Int)
        minSMM smm @?= 10
        maxSMM smm @?= 15
        min2SMM smm @?= 15
        max2SMM smm @?= 10
        nMinSMM smm @?= 1
        nMaxSMM smm @?= 1
        let s1 = sactWithLength (newAddACC (15 :: Int)) smm 2
        sumSMM s1 @?= 55
        let s2 = sactWithLength (newChminACC (5 :: Int)) smm 2
        sumSMM s2 @?= 10
        let s3 = sactWithLength (newChminACC (15 :: Int)) smm 2
        sumSMM s3 @?= 25
        let s4 = sactWithLength (newChmaxACC (15 :: Int)) smm 2
        sumSMM s4 @?= 30
        let s5 = sactWithLength (newChmaxACC (5 :: Int)) smm 2
        sumSMM s5 @?= 25,
      testCase "SumMinMax length 5" $ do
        let smm = singletonSMM (10 :: Int) <> singletonSMM (15 :: Int) <> singletonSMM (20 :: Int) <> singletonSMM (25 :: Int) <> singletonSMM (30 :: Int)
        minSMM smm @?= 10
        maxSMM smm @?= 30
        min2SMM smm @?= 15
        max2SMM smm @?= 25
        nMinSMM smm @?= 1
        nMaxSMM smm @?= 1
        let s1 = sactWithLength (newAddACC (15 :: Int)) smm 5
        sumSMM s1 @?= (10 + 15 + 20 + 25 + 30 + 15 * 5)
        let s2 = sactWithLength (newChminACC (5 :: Int)) smm 5
        sumSMM s2 @?= 5 * 5
        let s3 = sactWithLength (newChminACC (15 :: Int)) smm 5
        sumSMM s3 @?= (10 + 15 * 4)
        let s4 = sactWithLength (newChmaxACC (15 :: Int)) smm 5
        failsSMM s4 @?= True
        let s5 = sactWithLength (newChmaxACC (5 :: Int)) smm 5
        sumSMM s5 @?= (10 + 15 + 20 + 25 + 30)
        let s6 = sactWithLength (newChminACC (25 :: Int)) smm 5
        failsSMM s6 @?= True
        -- sumSMM s6 @?= (10 + 12 * 4)
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
        mempty <> newChmaxACC x @?= newChmaxACC x
    ]

tests :: [TestTree]
tests = [testSMM, testACC]
