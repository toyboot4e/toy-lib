{-# LANGUAGE CPP #-}
#include "./__import"
-- {{{ toy-lib import

import Data.Graph.Tree.LCT
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
  uvs <- U.replicateM (n - 1) ints2'
  qs <-
    U.replicateM q $
      int' >>= \case
        0 -> (0 :: Int,,,,) <$> int' <*> int' <*> int' <*> int'
        1 -> (1,,,-1,-1) <$> int' <*> int'
        2 -> (2,,,-1,-1) <$> int' <*> int'
        _ -> error "unreachable"

  lct <- buildWithInvOpLCT (\x -> -x) (U.map Sum xs) uvs
  res <- (`U.mapMaybeM` qs) $ \case
    (0, !u, !v, !w, !x) -> do
      -- delete edge (u, v)
      cutLCT lct u v
      -- add edge (w, x)
      linkLCT lct w x
      return Nothing
    (1, !p, !x, !_, !_) -> do
      -- add x
      modifyLCT lct (<> Sum x) p
      return Nothing
    (2, !v, !p, !_, !_) -> do
      -- output sub of subtree under @v@ whose parent is @p@.
      Sum res <- foldSubtreeLCT lct v p
      return $ Just res
    _ -> error "unreachable"

  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/dynamic_tree_vertex_add_subtree_sum
-- #lct
main :: IO ()
main = runIO solve
