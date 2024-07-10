{-# LANGUAGE CPP #-}
#include "./__import"

-- {{{ toy-lib import

import Data.DenseIntSet
import Data.SplayMap
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB

-- }}} toy-lib import

{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug = False

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !q) <- ints2'
  t <- U.findIndices (== '1') . U.fromList . BS.unpack <$> line'
  qs <- U.replicateM q ints2'

  sm <- newSMap n
  U.forM_ t $ \i -> do
    insertSMap_ sm i ()

  res <- (`U.mapMaybeM` qs) $ \case
    (0, !k) -> do
      insertSMap sm k ()
      return Nothing
    (1, !k) -> do
      deleteSMap sm k
      return Nothing
    (2, !k) -> do
      Just . bool (0 :: Int) 1 <$> memberSMap sm k
    (3, !k) -> do
      Just . maybe (-1) fst <$> lookupGESMap sm k
    (4, !k) -> do
      Just . maybe (-1) fst <$> lookupLESMap sm k
    _ -> error "unreachable"

  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/predecessor_problem
-- #dense-int-set
main :: IO ()
main = runIO solve
