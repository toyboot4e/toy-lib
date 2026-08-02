{-# LANGUAGE CPP #-}
#include "./__import"
-- {{{ toy-lib import

import Data.Vector.Extra
import ToyLib.DP
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB

-- }}} toy-lib import

{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug :: Bool
debug = False

solve :: StateT BS.ByteString IO ()
solve = do
  n <- intP
  xs <- intsP
  let (!_, !xs') = compressU xs
  let !res = lisOf' xs'
  printBSB $ G.length res
  -- no need to restore the value, we just need their index:
  printCol res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/longest_increasing_subsequence
-- #lis
main :: IO ()
main = runIO solve
