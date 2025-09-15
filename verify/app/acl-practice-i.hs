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
  printBSB $ countUniqueSubstrings s

-- verification-helper: PROBLEM https://atcoder.jp/contests/practice2/tasks/practice2_i
-- #suffix-array
--
-- Same as yosupo one.
main :: IO ()
main = runIO solve
