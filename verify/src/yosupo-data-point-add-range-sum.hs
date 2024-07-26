{-# LANGUAGE CPP #-}
#include "./__import"

-- {{{ toy-lib import
import Data.SegmentTree.Strict
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
  qs <- U.replicateM q ints3'

  stree <- buildSTree $ U.map Sum xs
  res <- (`U.mapMaybeM` qs) $ \case
    (0, !i, !dx) -> do
      modifySTree stree (<> Sum dx) i
      return Nothing
    (1, !l, pred -> !r) -> do
      Sum x <- foldSTree stree l r
      return $ Just x

  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/point_add_range_sum
-- #lazy-segment-tree #affine-2d
main :: IO ()
main = runIO solve
