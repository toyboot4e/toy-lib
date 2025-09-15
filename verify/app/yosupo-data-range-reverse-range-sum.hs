{-# LANGUAGE CPP #-}
#include "./__import"
-- {{{ toy-lib import

import Data.SplaySeq
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB

-- }}} toy-lib import

debug :: Bool
debug = False

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !q) <- ints2'

  -- TODO: handle correctly
  when (q == 0) $ do
    liftIO exitSuccess

  -- TODO: handle correctly
  when (n == 0) $ do
    qs <- U.replicateM q ints3'
    let nq = U.length $ U.filter ((== 1) . fst3) qs
    printBSB $ unlinesBSB $ U.replicate nq (0 :: Int)
    liftIO exitSuccess

  xs <- intsU'
  qs <- U.replicateM q ints3'

  seq <- newSS (n + q)
  allocSeqSS seq $ U.map Sum xs

  -- TODO: hide the varying root from the user
  res <- (`U.mapMaybeM` qs) $ \case
    (0, !l, pred -> !r) -> do
      reverseSS seq l r
      return Nothing
    (1, !l, pred -> !r) -> do
      Sum !x <- foldSS seq l r
      return $ Just x

  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/range_reverse_range_sum
-- #splay-seq
main :: IO ()
main = runIO solve
