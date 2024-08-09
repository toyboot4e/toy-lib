{-# LANGUAGE CPP #-}
#include "./__import"
-- {{{ toy-lib import

import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Extra (bindex)
import Data.WaveletMatrix
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB

-- }}} toy-lib import

{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug :: Bool
debug = True

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !q) <- ints2'
  xs <- intsU'
  lrks <- U.replicateM q ints3'

  let !dict = U.uniq $ U.modify VAI.sort xs
  let !xs' = U.map (bindex dict) xs
  let !wm = newWM n xs'

  let res =
        U.map
          ( \(!l, pred -> !r, !k) ->
              let !i = kthSmallestWM wm l r k
               in dict G.! i
          )
          lrks
  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/range_kth_smallest
-- #wavelet-matrix
main :: IO ()
main = runIO solve
