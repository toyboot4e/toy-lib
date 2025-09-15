{-# LANGUAGE CPP #-}
#include "./__import"

-- {{{ toy-lib import
import Data.ModInt
import Math.NTT
import Math.FloorSum
import ToyLib.Debug
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB

-- }}} toy-lib import
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug = False
-- }}}

solve :: StateT BS.ByteString IO ()
solve = do
  !t <- int'
  !input <- U.replicateM t ints4'
  printBSB . unlinesBSB $ U.map (\(!n, !m, !a, !b) -> floorSum n m a b) input

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/sum_of_floor_of_linear
-- #floor_sum
main :: IO ()
main = runIO solve
