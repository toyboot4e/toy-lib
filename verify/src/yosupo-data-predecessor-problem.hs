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
  t <- line'
  qs <- U.replicateM q ints2'

  set <- newDIS n
  forM_ (zip [0 .. ] (BS.unpack t)) $ \(!i, !c) -> do
    when (c == '1') $ do
      insertDIS set i

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
