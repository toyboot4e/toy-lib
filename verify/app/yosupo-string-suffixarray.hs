{-# LANGUAGE CPP #-}
#include "./__import"

-- {{{ toy-lib import
import Data.ByteString.SuffixArray
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB

-- }}} toy-lib import

{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug = False

solve :: StateT BS.ByteString IO ()
solve = do
  s <- line'
  printVec $ saOf s

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/suffixarray
-- #suffix-array
main :: IO ()
main = runIO solve
