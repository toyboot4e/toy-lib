{-# LANGUAGE CPP #-}
#include "./__import"

-- {{{ toy-lib import
import ToyLib.Debug
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB
import Data.Graph.Sparse
import Data.Graph.Tree.Hld

-- }}} toy-lib import
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug = False
-- }}}

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !q) <- ints2'
  ps <- intsU'
  qs <- U.replicateM q ints2'

  let !gr = buildSG n $ swapDupeU $ U.imap ((,) . succ) ps
  let !hld = hldOf gr

  printBSB . unlinesBSB $ U.map (uncurry (lcaHLD hld)) qs

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/lca
main :: IO ()
main = runIO solve
