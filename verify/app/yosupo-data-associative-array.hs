{-# LANGUAGE CPP #-}
#include "./__import"

-- {{{ toy-lib import

import Data.DenseHashMap
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB

-- }}} toy-lib import

{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug = False

-- 2830 ms.
solveContainers :: StateT BS.ByteString IO ()
solveContainers = do
  q <- int'
  qs <- U.replicateM q $ do
    int' >>= \case
      0 -> (0 :: Int,,) <$> int' <*> int'
      1 -> (1,,-1) <$> int'
      _ -> error "unreachable"

  res <- (`evalStateT` HM.empty) $ (`U.mapMaybeM` qs) $ \case
    (0, !k, !v) -> do
      modify' $ HM.insert k v
      return Nothing
    (1, !k, !_) -> do
      Just . fromMaybe 0 <$> gets (HM.lookup k)
    _ -> error "unreachable"

  printBSB $ unlinesBSB res

-- 426 ms. 1.5 times faster than index comporession + MVector.
solve :: StateT BS.ByteString IO ()
solve = do
  q <- int'
  qs <- U.replicateM q $ do
    int' >>= \case
      0 -> (0 :: Int,,) <$> int' <*> int'
      1 -> (1,,-1) <$> int'
      _ -> error "unreachable"

  hm <- newHM q
  res <- (`U.mapMaybeM` qs) $ \case
    (0, !k, !v) -> do
      writeHM hm k v
      return Nothing
    (1, !k, !_) -> do
      Just . fromMaybe 0 <$> readMayHM hm k
    _ -> error "unreachable"

  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/associative_array
-- #dense-hash-map
main :: IO ()
main = runIO solve
