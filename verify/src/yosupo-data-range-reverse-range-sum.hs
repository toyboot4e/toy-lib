{-# LANGUAGE CPP #-}
#include "./__import"
-- {{{ toy-lib import

import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB
import Data.SplaySeq

-- }}} toy-lib import

debug :: Bool
debug = True

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !q) <- ints2'
  xs <- intsU'
  qs <- U.replicateM q ints3'

  seq <- newSS (n + q)
  root0 <- allocSeqSS seq $ U.map Sum xs

  -- TODO: hide the varying root from the user
  res <- (`evalStateT` root0) . (`U.mapMaybeM` qs) $ \case
    (0, !l, pred -> !r) -> do
      root <- get
      root' <- reverseSS seq root l r
      put root'
      return Nothing
    (1, !l, pred -> !r) -> do
      root <- get
      (Sum !x, !root') <- foldSS seq root l r
      put root'
      return $ Just x

  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/range_reverse_range_sum
-- #splay-seq
main :: IO ()
main = runIO solve

