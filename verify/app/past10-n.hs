{-# LANGUAGE CPP #-}
#include "./__import"

import Data.ModInt
import Math.NTT
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB

-- }}} toy-lib import
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug :: Bool
debug = False

{- ORMOLU_DISABLE -}
type MyModulo = (2305843009213693951 :: Nat) -- 2^61-1
type MyModInt = ModInt MyModulo ; myMod :: Int ; myMod = fromInteger $ natVal' @MyModulo proxy# ; {-# INLINE modInt #-} ; modInt :: Int -> MyModInt ; modInt = ModInt . (`rem` myMod) ;
{- ORMOLU_ENABLE -}

solve :: StateT BS.ByteString IO ()
solve = do
  n <- int'
  let !m = 200000 :: Int
  xs <- intsU'
  let !ps = U.accumulate (+) (U.replicate m (0 :: Int)) $ U.map ((, 1) . pred) xs
  let !qs = U.accumulate (+) (U.replicate m (0 :: Int)) $ U.map ((, 1) . (m -)) xs
  let !res = convolute64 ps qs
  printBSB $ U.length $ U.filter (/= 0) res

-- verification-helper: PROBLEM https://atcoder.jp/contests/past202203-open/tasks/past202203_n
-- #FFT
main :: IO ()
main = runIO solve

