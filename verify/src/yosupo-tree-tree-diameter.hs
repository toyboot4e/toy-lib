{-# LANGUAGE CPP #-}
#include "./__import"

-- {{{ toy-lib import

import Data.Graph.Sparse
import Data.Graph.Tree.TreeSG
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB

-- }}} toy-lib import
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug :: Bool
debug = True

solve :: StateT BS.ByteString IO ()
solve = do
  n <- int'
  es <- U.replicateM (n - 1) ints3'
  let !gr = buildWSG n $ swapDupeW es
  let ((!_, !_, !w), !path) = treeDiameterPathSG gr (-1)
  printBSB (w, U.length path)
  printVec path

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/tree_diameter
-- #mst
main :: IO ()
main = runIO solve
