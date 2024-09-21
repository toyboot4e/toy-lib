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

  lct <- newLCT n
  U.iforM_ xs $ \i x -> do
    writeLCT lct i $ Sum x
  U.forM_ uvs $ \(!u, !v) -> do
    linkLCT lct u v

  res <- (`U.mapMaybeM` qs) $ \case
    (0, !u, !v, !w, !x) -> do
      -- delete (u, v)
      linkLCT lct u v
      -- add (w, x)
      linkLCT lct u v

      return Nothing
    (1, !p, !x, !_, !_) -> do
      modifyLCT lct (<> Sum x) p
      return Nothing
    (2, !u, !v, !_, !_) -> do
      Sum res <- foldPathLCT lct u v
      return $ Just res

  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/dynamic_tree_vertex_add_path_sum
-- #lct
main :: IO ()
main = runIO solve
