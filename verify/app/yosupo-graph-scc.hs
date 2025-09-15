{-# LANGUAGE CPP #-}
#include "./__import"

-- {{{ toy-lib import

import Data.Graph.Sparse
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB

-- }}} toy-lib import
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug :: Bool
debug = False

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !m) <- ints2'
  es <- U.replicateM m ints2'

  let gr = buildSG n es
  let sccs = downSccSG gr
  let sccs' = V.fromList $ map (U.fromList . reverse) sccs

  printBSB $ V.length sccs'
  printBSB $ intersperseWithBSB ((<>) <$> (<> wsBSB) . showBSB . G.length <*> unwordsBSB) endlBSB sccs'

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/scc
-- #scc
main :: IO ()
main = runIO solve
