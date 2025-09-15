{-# LANGUAGE CPP #-}
#include "./__import"

-- {{{ toy-lib import

import Data.Graph.Sparse
import Data.Graph.Tree.Hld
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB

-- }}} toy-lib import
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug :: Bool
debug = False

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !q) <- ints2'
  xs <- intsU'
  ps <- intsU'
  qs <- U.replicateM q $ do
    int' >>= \case
      0 -> (0 :: Int,,) <$> int' <*> int'
      1 -> (1 :: Int,,-1) <$> int'
      _ -> error "unreachable"

  let es = U.imap (\(succ -> to) from -> (from, to)) ps
  let gr = buildSG_ n $ swapDupeU es
  let hld = hldOf' gr 0
  tm <- buildVertTM hld True $ U.map Sum xs
  res <- (`U.mapMaybeM` qs) $ \case
    (0, !i, !x) -> do
      modifyTM tm (<> Sum x) i
      return Nothing
    (1, !r, !_) -> do
      Just <$> foldSubtreeVertsTM tm r
    _ -> error "unreachable"
  printBSB . unlinesBSB $ U.map getSum res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/vertex_add_subtree_sum
-- #hld
main :: IO ()
main = runIO solve
