{-# LANGUAGE CPP #-}
#include "./__import"
-- {{{ toy-lib import
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB
import ToyLib.DP
import Data.Vector.Extra (bindex)
-- }}} toy-lib import

{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug :: Bool
debug = False

solve :: StateT BS.ByteString IO ()
solve = do
  n <- int'
  xs <- intsU'
  let !dict = U.uniq $ U.modify VAI.sort xs
  let !res = lisOf' $ U.map (bindex dict) xs
  printBSB $ G.length res
  -- no need to restore the value, we just need their index:
  -- printVec $ U.backpermute dict res
  printVec res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/longest_increasing_subsequence
-- #lis
main :: IO ()
main = runIO solve
