{-# LANGUAGE CPP #-}
#include "./__import"
-- {{{ toy-lib import

import Data.SplayMap
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
  t <- U.findIndices (== '1') . U.fromList . BS.unpack <$> line'
  qs <- U.replicateM q ints2'

  sm <- newSM n
  U.forM_ t $ \i -> do
    insertSM_ sm i ()

  res <- (`U.mapMaybeM` qs) $ \case
    (0, !k) -> do
      insertSM_ sm k ()
      return Nothing
    (1, !k) -> do
      deleteSM_ sm k
      return Nothing
    (2, !k) -> do
      Just . bool (0 :: Int) 1 <$> memberSM sm k
    (3, !k) -> do
      Just . maybe (-1) fst <$> lookupGESM sm k
    (4, !k) -> do
      Just . maybe (-1) fst <$> lookupLESM sm k
    _ -> error "unreachable"

  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/predecessor_problem
-- #splay-tree
main :: IO ()
main = runIO solve
