{-# LANGUAGE CPP #-}
#include "./__import"
-- {{{ toy-lib import

import Data.ModInt
import Data.Vector.IxVector
import Math.Matrix
import ToyLib.Parser
import ToyLib.Parser.Grid
import ToyLib.Prelude
import ToyLib.ShowBSB
import ToyLib.ShowBSB.Grid
import Math.Stimes

-- }}} toy-lib import

{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug :: Bool
debug = False

{- ORMOLU_DISABLE -}
type MyModulo = (998244353 :: Nat) -- (1_000_000_007 :: Nat)
type MyModInt = ModInt MyModulo ; myMod :: Int ; myMod = fromInteger $ natVal' @MyModulo proxy# ; {-# INLINE modInt #-} ; modInt :: Int -> MyModInt ; modInt = ModInt . (`rem` myMod) ;
{- ORMOLU_ENABLE -}

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !k) <- ints2'
  mat <- mapIV (ModInt @MyModulo) <$> getMat' n n
  let !mat'
        | k == 0 = unitMat n
        | otherwise = stimes' k mat
  printMat mat'

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/pow_of_matrix
-- #matrix
main :: IO ()
main = runIO solve
