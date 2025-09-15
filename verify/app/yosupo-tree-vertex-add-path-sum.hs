{-# LANGUAGE CPP #-}
#include "./__import"

-- {{{ toy-lib

import Data.Graph.Sparse
import Data.Graph.Tree.Hld
import Data.SegmentTree.Strict
import ToyLib.Debug
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB

-- }}} toy-lib
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug = False

-- }}}

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !q) <- ints2'
  xs <- intsU'
  es <- U.replicateM (n - 1) ints2'
  qs <- U.replicateM q ints3'

  let !gr = buildSG n $ swapDupeU es
  let !hld = hldOf gr
  tm <- buildVertTM hld True $ U.map Sum xs

  res <- (`U.mapMaybeM` qs) $ \case
    (0, !p, !x) -> do
      modifyTM tm (<> Sum x) p
      return Nothing
    (1, !u, !v) -> do
      Just . getSum <$> foldTM tm u v
    _ -> error "unreachable"

  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/vertex_add_path_sum
main :: IO ()
main = runIO solve
