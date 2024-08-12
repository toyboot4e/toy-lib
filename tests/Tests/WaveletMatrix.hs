module Tests.WaveletMatrix where

import Algorithm.Bisect
import Data.Bit
import Data.Ix
import Data.Maybe
import Data.Ord
import Data.Tuple.Extra
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Extra
import Data.Vector.Generic qualified as G
import Data.Vector.Unboxed qualified as U
import Data.WaveletMatrix
import Data.WaveletMatrix.Raw
import Data.WaveletMatrix.SuccinctDictionary
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck ((.&&.))
import Test.Tasty.QuickCheck as QC

fixedTest :: TestTree
fixedTest =
  testGroup
    "Wavelet Matrix fixed test"
    [ testCase "example" $ do
        -- The example in this article:
        -- https://miti-7.hatenablog.com/entry/2018/04/28/152259
        let xs = U.fromList [5 :: Int, 4, 5, 5, 2, 1, 5, 6, 1, 3, 5, 0]

        let bits =
              V.map U.fromList $
                V.fromList
                  [ [1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0],
                    [1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0],
                    [1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0]
                  ]

        let ints =
              V.map U.fromList $
                V.fromList
                  [ [1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0],
                    [1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0],
                    [1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0]
                  ]

        let wm = newRWM 6 xs
        bitsRWM wm @?= bits

        let nZeros = U.convert $ V.map ((U.length xs -) . U.sum) ints
        nZerosRWM wm @?= nZeros

        let csums = V.map (U.cons 0 . U.singleton . U.sum) ints
        csumsRWM wm @?= csums

        let xs' = U.generate (U.length xs) (accessRWM wm)
        xs' @?= xs
    ]

findKthIndex :: (Eq a, U.Unbox a) => Int -> a -> U.Vector a -> Maybe Int
findKthIndex k x xs = fmap fst . (G.!? k) . U.filter ((== x) . snd) $ U.indexed xs

dictTests :: TestTree
dictTests =
  testGroup
    "Succinct dictinary funciton tests"
    [ QC.testProperty "freq" $ do
        n <- QC.chooseInt (1, maxN)
        xs <- U.fromList . map Bit <$> QC.vectorOf n (QC.chooseEnum (False, True))
        let csum = csumBV xs
        let expected = (U.length (U.filter (== 0) xs), U.length (U.filter (== 1) xs))
        let res = (freq0BV xs csum n, freq1BV xs csum n)
        return $ res QC.=== expected,
      QC.testProperty "findKthIndex" $ do
        n <- QC.chooseInt (1, maxN)
        xs <- U.fromList . map Bit <$> QC.vectorOf n (QC.chooseEnum (False, True))
        k <- QC.chooseInt (0, n - 1)
        x <- Bit <$> QC.chooseEnum (False, True)

        let expected = findKthIndex k x xs

        let csum = csumBV xs
        let res
              | x == 0 = findKthIndex0BV xs csum k
              | otherwise = findKthIndex1BV xs csum k

        return . QC.counterexample (show (k, x, xs)) $ res QC.=== expected
    ]
  where
    maxN = 256

sliceLR :: Int -> Int -> U.Vector Int -> U.Vector Int
sliceLR l r = U.take (r - l + 1) . U.drop l

randomTests :: TestTree
randomTests =
  testGroup
    "Wavelet Matrix random tests"
    [ QC.testProperty "kthMin" $ do
        n <- QC.chooseInt (1, maxN)
        xs <- U.fromList <$> QC.vectorOf n (QC.chooseInt rng)

        l <- QC.chooseInt (0, n - 1)
        r <- QC.chooseInt (l, n - 1)

        k <- QC.chooseInt (0, r - l)
        let slice = sliceLR l r xs
        let expected =
              let (!i, !x) = U.modify (VAI.sortBy (comparing swap)) (U.indexed slice) G.! k
               in (i + l, x)

        -- TODO: automatic index compression
        let wm = newWM xs
        let (!i, !x) = ikthMinWM wm l r k

        return . QC.counterexample (show ((l, r, k), xs)) $
          ikthMinWM wm l r k QC.=== expected .&&. kthMinWM wm l r k QC.=== snd expected,
      QC.testProperty "kth index" $ do
        !n <- QC.chooseInt (1, maxN)
        !xs <- U.fromList <$> QC.vectorOf n (QC.chooseInt rng)

        !k <- QC.chooseInt (0, min (n - 1) 10)
        !x <- (xs G.!) <$> QC.chooseInt (0, n - 1)

        let !ixs = U.indexed xs
        let !expected = fmap fst . (G.!? k) $ U.filter ((== x) . snd) ixs

        let !wm = newWM xs
        let !res = findKthIndexWM wm k x

        return . QC.counterexample (show (k, x, xs)) $ res QC.=== expected,
      QC.testProperty "freq" $ do
        !n <- QC.chooseInt (1, maxN)
        !xs <- U.fromList <$> QC.vectorOf n (QC.chooseInt rng)

        l <- QC.chooseInt (0, n - 1)
        r <- QC.chooseInt (l, n - 1)
        xl <- QC.chooseInt rng
        xr <- QC.chooseInt (xl, snd rng)

        let slice = sliceLR l r xs
        let expected = U.length $ U.filter (inRange (xl, xr)) slice

        let !wm = newWM xs
        let !res = freqInWM wm l r xl xr
        return . QC.counterexample (show ((l, r), (xl, xr), xs)) $ res QC.=== expected
    ]
  where
    rng = (-20, 20) :: (Int, Int)
    maxN = 256

tests :: [TestTree]
tests = [testGroup "Data.WaveletMatrix" [fixedTest, dictTests, randomTests]]
