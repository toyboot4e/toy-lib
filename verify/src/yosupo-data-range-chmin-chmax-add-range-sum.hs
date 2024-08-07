{-# LANGUAGE CPP #-}
#include "./__import"
-- {{{ toy-lib import
import Data.SegmentTree.Beats
import Data.SegmentTree.Beats.SumMinMax
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB
-- }}} toy-lib import

{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug :: Bool
debug = True

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !q) <- ints2'
  xs <- intsU'
  qs <- U.replicateM q $ int' >>= \case
    0 -> (0 :: Int,,,) <$> int' <*> int1' <*> int'
    1 -> (1 :: Int,,,) <$> int' <*> int1' <*> int'
    2 -> (2 :: Int,,,) <$> int' <*> int1' <*> int'
    3 -> (3 :: Int,,,-1) <$> int' <*> int1'
    _ -> error "unreachable"

  stree <- buildSTB $ U.map singletonSMM xs
  res <- (`U.mapMaybeM` qs) $ \(c, !l, !r, !x) -> case c of
    0 -> do
      sactSTB stree l r $ newChminACC x
      return Nothing
    1 -> do
      sactSTB stree l r $ newChmaxACC x
      return Nothing
    2 -> do
      sactSTB stree l r $ newAddACC x
      return Nothing
    3 -> do
      Just . sumSMM <$> foldSTB stree l r
    _ -> error "unreachable"

  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/range_chmin_chmax_add_range_sum
-- #segment-tree-beats
main :: IO ()
main = runIO solve
