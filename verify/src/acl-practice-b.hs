{-# LANGUAGE CPP #-}
#include "./__import"
import Data.Coerce
-- {{{ toy-lib import
import Data.FenwickTree
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB
import ToyLib.ShowBSB.Grid

-- }}} toy-lib import

{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug :: Bool
debug = False

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !q) <- ints2'
  xs <- intsU'
  qs <- U.replicateM q ints3'

  ft <- buildFT @(Sum Int) $ U.map coerce xs
  res <- (`U.mapMaybeM` qs) $ \case
    (0, !p, !x) -> do
      addFT ft p $ coerce x
      return Nothing
    (1, !l, !r) -> do
      x <- sumFT ft l $ r - 1
      return $ Just $ getSum x
    _ -> error "unreachable"

  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://atcoder.jp/contests/practice2/tasks/practice2_b
-- #fenwick-tree
main :: IO ()
main = runIO solve
