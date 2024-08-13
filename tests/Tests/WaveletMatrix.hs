module Tests.WaveletMatrix where

import Data.Bit
import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
import Data.Ix
import Data.Ord
import Data.Tuple.Extra
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Generic qualified as G
import Data.Vector.Unboxed qualified as U
import Data.WaveletMatrix
import Data.WaveletMatrix.BitVector
import Data.WaveletMatrix.Raw
import Test.Tasty
import Test.Tasty.HUnit
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
        V.map bitsBV (bitsRWM wm) @?= bits

        let nZeros = U.convert $ V.map ((U.length xs -) . U.sum) ints
        nZerosRWM wm @?= nZeros

        let csums = V.map (U.cons 0 . U.singleton . U.sum) ints
        V.map csumBV (bitsRWM wm) @?= csums

        let xs' = U.generate (U.length xs) (accessRWM wm)
        xs' @?= xs
    ]

findKthIndex :: (Eq a, U.Unbox a) => Int -> a -> U.Vector a -> Maybe Int
findKthIndex k x xs = fmap fst . (G.!? k) . U.filter ((== x) . snd) $ U.indexed xs

lrFindKthIndex :: (Eq a, U.Unbox a) => Int -> a -> Int -> Int -> U.Vector a -> Maybe Int
lrFindKthIndex k x l r xs = fmap fst . (G.!? k) . U.filter ((== x) . snd) . sliceLR l r $ U.indexed xs

bitVectorTests :: TestTree
bitVectorTests =
  testGroup
    "BitVector function tests"
    [ QC.testProperty "freq" $ do
        n <- QC.chooseInt (1, maxN)
        xs <- U.fromList . map Bit <$> QC.vectorOf n (QC.chooseEnum (False, True))
        let expected = (U.length (U.filter (== 0) xs), U.length (U.filter (== 1) xs))

        let bv = BitVector xs $ newCSumBV xs
        let res = (freq0BV bv n, freq1BV bv n)
        return $ res QC.=== expected,
      QC.testProperty "findKthIndex" $ do
        n <- QC.chooseInt (1, maxN)
        xs <- U.fromList . map Bit <$> QC.vectorOf n (QC.chooseEnum (False, True))
        k <- QC.chooseInt (0, n - 1)
        x <- Bit <$> QC.chooseEnum (False, True)
        let expected = findKthIndex k x xs

        let bv = BitVector xs $ newCSumBV xs
        let res
              | x == 0 = findKthIndex0BV bv k
              | otherwise = findKthIndex1BV bv k

        return . QC.counterexample (show (k, x, xs)) $ res QC.=== expected,
      QC.testProperty "lrFindKthIndex" $ do
        n <- QC.chooseInt (1, maxN)
        xs <- U.fromList . map Bit <$> QC.vectorOf n (QC.chooseEnum (False, True))
        k <- QC.chooseInt (0, n - 1)
        x <- Bit <$> QC.chooseEnum (False, True)
        l <- QC.chooseInt (0, n - 1)
        r <- QC.chooseInt (l, n - 1)

        let expected = lrFindKthIndex k x l r xs

        let bv = BitVector xs $ newCSumBV xs
        let res
              | x == 0 = lrFindKthIndex0BV bv k l r
              | otherwise = lrFindKthIndex1BV bv k l r

        return . QC.counterexample (show (k, x, (l, r), xs)) $ res QC.=== expected
    ]
  where
    maxN = 256

sliceLR :: (U.Unbox a) => Int -> Int -> U.Vector a -> U.Vector a
sliceLR l r = U.take (r - l + 1) . U.drop l

-- | Generates a random sequence with a slice.
genLR :: Int -> (Int, Int) -> Gen (Int, U.Vector Int, (Int, Int), U.Vector Int)
genLR maxN rng = do
  n <- QC.chooseInt (1, maxN)
  xs <- U.fromList <$> QC.vectorOf n (QC.chooseInt rng)
  l <- QC.chooseInt (0, n - 1)
  r <- QC.chooseInt (l, n - 1)
  return (n, xs, (l, r), sliceLR l r xs)

randomTests :: TestTree
randomTests =
  testGroup
    "Wavelet Matrix random tests"
    [ QC.testProperty "kthMin" $ do
        (!_, !xs, (!l, !r), !slice) <- genLR maxN rng
        k <- QC.chooseInt (0, r - l)
        let sorted = U.modify (VAI.sortBy (comparing swap)) (U.indexed slice)
        let expected =
              let (!i, !x) = sorted G.! k
               in Just (i + l, x)

        let wm = newWM xs
        return . QC.counterexample (show ((l, r, k), xs)) $
          ikthMinWM wm l r k QC.=== expected .&&. kthMinWM wm l r k QC.=== (snd <$> expected),
      QC.testProperty "kthMax" $ do
        (!_, !xs, (!l, !r), !_) <- genLR maxN rng
        k <- QC.chooseInt (0, r - l)
        let slice = sliceLR l r xs
        let sorted = U.modify (VAI.sortBy (comparing (Down . swap))) (U.indexed slice)
        let expected =
              let (!i, !x) = sorted G.! k
               in Just (i + l, x)

        let wm = newWM xs
        return . QC.counterexample (show ((l, r, k), xs)) $
          ikthMaxWM wm l r k QC.=== expected .&&. kthMaxWM wm l r k QC.=== (snd <$> expected),
      QC.testProperty "kth index in [0, n)" $ do
        (!n, !xs, (!_, !_), !_) <- genLR maxN rng
        !k <- QC.chooseInt (0, min (n - 1) 10)
        !x <- (xs G.!) <$> QC.chooseInt (0, n - 1)

        let !ixs = U.indexed xs
        let !expected = fmap fst . (G.!? k) $ U.filter ((== x) . snd) ixs

        let !wm = newWM xs
        let !res = findKthIndexWM wm k x

        return . QC.counterexample (show (k, x, xs)) $ res QC.=== expected,
      QC.testProperty "kth index in [l, r]" $ do
        (!n, !xs, (!l, !r), !_) <- genLR maxN rng
        !k <- QC.chooseInt (0, min (n - 1) 10)
        !x <- (xs G.!) <$> QC.chooseInt (0, n - 1)

        let !ixs = U.indexed xs
        let !expected = fmap fst . (G.!? k) . U.filter ((== x) . snd) . sliceLR l r $ ixs

        let !wm = newWM xs
        let !res = lrFindKthIndexWM wm k x l r

        return . QC.counterexample (show (k, x, (l, r), xs)) $ res QC.=== expected,
      QC.testProperty "freq" $ do
        (!_, !xs, (!l, !r), !slice) <- genLR maxN rng
        xl <- QC.chooseInt rng
        xr <- QC.chooseInt (xl, snd rng)

        let expected = U.length $ U.filter (inRange (xl, xr)) slice

        let !wm = newWM xs
        let !res = freqInWM wm l r xl xr
        return . QC.counterexample (show ((l, r), (xl, xr), xs)) $ res QC.=== expected,
      QC.testProperty "lookupLE, lookupGE" $ do
        (!_, !xs, (!l, !r), !slice) <- genLR maxN rng
        x <- QC.chooseInt rng
        queryKind <- QC.chooseInt (0, 3)

        let im = IS.fromList $ U.toList slice
        let expected = case queryKind of
              0 -> IS.lookupLE x im
              1 -> IS.lookupLT x im
              2 -> IS.lookupGE x im
              3 -> IS.lookupGT x im
              _ -> error "unreachable"

        let !wm = newWM xs
        let !res = case queryKind of
              0 -> lookupLEWM wm l r x
              1 -> lookupLTWM wm l r x
              2 -> lookupGEWM wm l r x
              3 -> lookupGTWM wm l r x
              _ -> error "unreachable"

        return . QC.counterexample (show (x, queryKind, (l, r), xs)) $ res QC.=== expected,
      QC.testProperty "assocs" $ do
        (!_, !xs, (!l, !r), !slice) <- genLR maxN rng

        let expected = IM.assocs . IM.fromListWith (+) . U.toList $ U.map (,1 :: Int) slice
        let wm = newWM xs
        let res = assocsWM wm l r
        return . QC.counterexample (show ((l, r), xs)) $ res QC.=== expected,
      QC.testProperty "descAssocs" $ do
        (!_, !xs, (!l, !r), !slice) <- genLR maxN rng

        let expected = reverse . IM.assocs . IM.fromListWith (+) . U.toList $ U.map (,1 :: Int) slice
        let wm = newWM xs
        let res = descAssocsWM wm l r
        return . QC.counterexample (show ((l, r), xs)) $ res QC.=== expected
    ]
  where
    rng = (-20, 20) :: (Int, Int)
    maxN = 256

tests :: [TestTree]
tests = [testGroup "Data.WaveletMatrix" [fixedTest, bitVectorTests, randomTests]]
