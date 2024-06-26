{-# LANGUAGE CPP #-}
#include "./__import"

-- {{{ toy-lib import
import Data.Graph.Sparse
import Data.Graph.Tree.Hld
import Data.Graph.Tree.Lca
import Data.Graph.Tree.TreeSG
import ToyLib.Debug
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB

-- }}} toy-lib import
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug = False

-- }}}

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !q) <- ints2'
  ps <- intsU'
  qs <- U.replicateM q ints2'

  -- FIXME:
  let !gr = buildSG_ n $ swapDupeU $ U.imap ((,) . (+ 1)) ps
  let !lcaCache = lcaCacheSG gr 0

  printBSB . unlinesBSB $ U.map (fst . uncurry (lca lcaCache)) qs

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/lca
main :: IO ()
main = runIO solve
