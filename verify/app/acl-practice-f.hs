{-# LANGUAGE CPP #-}
#include "./__import"

-- {{{ toy-lib import

import Data.ModInt
import Math.NTT
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB

-- }}} toy-lib import
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug :: Bool
debug = False

-- }}}

{- ORMOLU_DISABLE -}
type MyModulo = (998244353 :: Nat) -- (1_000_000_007 :: Nat)
type MyModInt = ModInt MyModulo ; myMod :: Int ; myMod = fromInteger $ natVal' @MyModulo proxy# ; {-# INLINE modInt #-} ; modInt :: Int -> MyModInt ; modInt = ModInt . (`rem` myMod) ;
{- ORMOLU_ENABLE -}

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !m) <- ints2'
  xs <- U.map modInt <$> intsU'
  ys <- U.map modInt <$> intsU'
  printVec $ convoluteMod xs ys

-- verification-helper: PROBLEM https://atcoder.jp/contests/practice2/tasks/practice2_f
-- #FFT
main :: IO ()
main = runIO solve
