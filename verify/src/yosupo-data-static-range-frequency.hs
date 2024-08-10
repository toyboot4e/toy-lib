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

  let !dict = U.uniq $ U.modify VAI.sort xs
  let !wm = newWM (G.length dict) $ U.map (bindex dict) xs

  let res =
        U.map
          ( \(!l, !r, !x) ->
              let !i = bsearchL dict (<= x)
                  !x' = maybe (-1) (dict G.!) i
               in if x' == x
                    then freqWM wm l (r - 1) $ fromJust i
                    else 0
          )
          lrxs
  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/static_range_frequency
-- #wavelet-matrix
main :: IO ()
main = runIO solve
