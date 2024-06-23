{-# LANGUAGE CPP #-}
#include "./__import"

-- {{{ toy-lib import

import Data.DenseIntSet
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

  -- TODO: build function
  set <- newDIS n
  U.forM_ t $ insertDIS set

  res <- (`U.mapMaybeM` qs) $ \case
    (0, !k) -> do
      insertDIS set k
      return Nothing
    (1, !k) -> do
      deleteDIS set k
      return Nothing
    (2, !k) -> do
      Just . bool (0 :: Int) 1 <$> memberDIS set k
    (3, !k) -> do
      Just . fromMaybe (-1) <$> lookupGEDIS set k
    (4, !k) -> do
      Just . fromMaybe (-1) <$> lookupLEDIS set k
    _ -> error "unreachable"

  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/predecessor_problem
-- #fast-set
main :: IO ()
main = runIO solve
