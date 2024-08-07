{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}

module Tests.PUnionFind where

import Data.Instances.Affine2d
import Data.ModInt
import Data.Semigroup
import Data.UnionFind.Potencial
import GHC.Exts (proxy#)
import GHC.TypeLits
import Test.Tasty
import Test.Tasty.HUnit

{- ORMOLU_DISABLE -}
type MyModulo = (998244353 :: Nat) -- (1_000_000_007 :: Nat)
type MyModInt = ModInt MyModulo ; myMod :: Int ; myMod = fromInteger $ natVal' @MyModulo proxy# ; {-# INLINE modInt #-} ; modInt :: Int -> MyModInt ; modInt = ModInt . (`rem` myMod) ;
{- ORMOLU_ENABLE -}

props :: TestTree
props =
  testGroup
    "PUnionFind test"
    [ testCase "simple non-commutative" $ do
        let a = Mat2x2 (modInt 1, 2, 3, 4)
        let b = Mat2x2 (modInt 5, 6, 7, 8)

        uf <- newPUF 5
        unifyPUF_ uf 1 0 a
        unifyPUF_ uf 2 1 b

        x <- diffPUF uf 2 0
        x @?= b <> a

        y <- diffPUF uf 1 0
        y @?= a,
      testCase "simple non-commutative, but Dual" $ do
        let a = Mat2x2 (modInt 1, 2, 3, 4)
        let b = Mat2x2 (modInt 5, 6, 7, 8)

        uf <- newPUF 5
        unifyPUF_ uf 1 0 $ Dual a
        unifyPUF_ uf 2 1 $ Dual b

        Dual x <- diffPUF uf 2 0
        x @?= a <> b

        Dual y <- diffPUF uf 1 0
        y @?= a
    ]

tests :: [TestTree]
tests = [testGroup "Data.UnionFind.Potencial" [props]]
