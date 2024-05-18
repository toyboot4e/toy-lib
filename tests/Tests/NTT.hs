{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}

module Tests.NTT where

import Data.ModInt
import Data.Vector.Unboxed qualified as U
import GHC.TypeLits
import Math.NTT
import Test.Tasty
import Test.Tasty.QuickCheck as QC

inttNtt :: (KnownNat p) => U.Vector (ModInt p) -> QC.Property
inttNtt xs = U.take (U.length xs) (intt (ntt xs)) QC.=== xs

-- TODO: @ntt . intt = id@ if the index sort is removed
-- nttIntt :: (KnownNat p) => U.Vector (ModInt p) -> QC.Property
-- nttIntt xs = U.take (U.length xs) (ntt (intt xs)) QC.=== xs

props :: TestTree
props =
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

tests :: [TestTree]
tests = [testGroup "Math.NTT" [props]]
