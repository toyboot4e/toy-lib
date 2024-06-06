{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}

module Tests.NTT where

import Data.Bits
import Data.ModInt
import Data.Vector.Unboxed qualified as U
import GHC.TypeLits
import Math.NTT
import Test.Tasty
import Test.Tasty.QuickCheck as QC

naiveBitReverse :: Int -> Int
naiveBitReverse x0 = U.ifoldl' f (0 :: Int) $ U.reverse $ U.generate 32 $ testBit x0
  where
    f acc _ False = acc
    f acc i True = setBit acc i

bitReverseProps :: TestTree
bitReverseProps =
  testGroup
    "Bit reverse properties"
    [ QC.testProperty "implementation" $ do
        x <- QC.chooseInt (0, bit 32 - 1)
        return $ naiveBitReverse x QC.=== bitReverse x
    ]

inttNtt :: (KnownNat p) => U.Vector (ModInt p) -> QC.Property
inttNtt xs = U.take (U.length xs) (intt (ntt xs)) QC.=== xs

-- TODO: @ntt . intt = id@ if the index sort is removed
-- nttIntt :: (KnownNat p) => U.Vector (ModInt p) -> QC.Property
-- nttIntt xs = U.take (U.length xs) (ntt (intt xs)) QC.=== xs

-- \(O(N^2)\) Convolution.
naiveConvolute :: forall p. (KnownNat p) => U.Vector (ModInt p) -> U.Vector (ModInt p) -> U.Vector (ModInt p)
naiveConvolute xs1 xs2 = U.generate (len1 + len2 - 1) f
  where
    len1 = U.length xs1
    len2 = U.length xs2
    f :: Int -> ModInt p
    f i = U.sum $ (`U.imapMaybe` xs1) $ \i1 x1 -> do
      let i2 = i - i1
      x2 <- xs2 U.!? i2
      return $ x1 * x2

nttProps :: TestTree
nttProps =
  testGroup
    "NTT properties"
    [ QC.testProperty "intt . ntt" $ do
        n <- QC.chooseInt (1, 100)
        xs <- U.fromList . map (ModInt @998244353) <$> QC.vectorOf n (QC.chooseInt (0, 998244353 - 1))
        return $ inttNtt xs
        -- QC.testProperty "ntt . intt" $ do
        --   n <- QC.chooseInt (1, 100)
        --   xs <- U.fromList . map (ModInt @998244353) <$> QC.vectorOf n (QC.chooseInt (0, 998244353 - 1))
        --   return $ nttIntt xs
    ]

convoluteProps :: TestTree
convoluteProps =
  testGroup
    "Convolute properties"
    [ QC.testProperty "convolution (stupid == FFT)" $ do
        let size = 256
        n1 <- QC.chooseInt (1, size)
        xs1 <- U.fromList . map (ModInt @998244353) <$> QC.vectorOf n1 (QC.chooseInt (0, 998244353 - 1))
        n2 <- QC.chooseInt (1, size)
        xs2 <- U.fromList . map (ModInt @998244353) <$> QC.vectorOf n2 (QC.chooseInt (0, 998244353 - 1))
        return $ convoluteMod xs1 xs2 QC.=== naiveConvolute xs1 xs2
    ]

tests :: [TestTree]
tests = [testGroup "Math.NTT" [bitReverseProps, nttProps, convoluteProps]]
