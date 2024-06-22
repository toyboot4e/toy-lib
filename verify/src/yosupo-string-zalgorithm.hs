{-# LANGUAGE CPP #-}
#include "./__import"

-- {{{ toy-lib import
import Data.ByteString.ZFunction
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB
-- }}} toy-lib import

{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug = False

solve :: StateT BS.ByteString IO ()
solve = do
  s <- line'
  printVec $ zOf s

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/zalgorithm
-- #suffix-array
main :: IO ()
main = runIO solve
