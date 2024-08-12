{-# LANGUAGE CPP #-}
#include "./__import"
-- {{{ toy-lib import

import Algorithm.Bisect
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

  when (n == 0) $ do
    printBSB $ unlinesBSB $ U.replicate q '0'
    liftIO exitSuccess

  xs <- intsU'
  lrxs <- U.replicateM q ints3'

  let !wm = newWM xs
  let res = U.map (\(!l, !r, !x) -> freqWM wm l (r - 1) x) lrxs
  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/static_range_frequency
-- #wavelet-matrix
main :: IO ()
main = runIO solve
