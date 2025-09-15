{-# LANGUAGE CPP #-}
#include "./__import"
-- {{{ toy-lib import

import Data.Vector.Extra
import Data.WaveletMatrix
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB

-- }}} toy-lib import

{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug :: Bool
debug = False

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !q) <- ints2'
  xs <- intsU'
  lrks <- U.replicateM q ints3'

  let !wm = buildWM xs
  let res = U.map (\(!l, !r, !k) -> fromJust $ kthMinWM wm l (r - 1) k) lrks
  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/range_kth_smallest
-- #wavelet-matrix
main :: IO ()
main = runIO solve
