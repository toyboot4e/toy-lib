{-# LANGUAGE CPP #-}
#include "./__import"

-- {{{ toy-lib import

import Data.UnionFind.Mutable
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB

-- }}} toy-lib import

{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug = False

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !q) <- ints2'
  qs <- U.replicateM q $ do
    int' >>= \case
      0 -> (0 :: Int,,) <$> int' <*> int'
      1 -> (1 :: Int,,) <$> int' <*> int'
      _ -> error "unreachable"

  uf <- newMUF n
  res <- (`U.mapMaybeM` qs) $ \case
    (0, !u, !v) -> do
      unifyMUF_ uf u v
      return Nothing
    (1, !u, !v) -> do
      Just . bool (0 :: Int) 1 <$> sameMUF uf u v
    _ -> error "unreachable"

  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/unionfind
main :: IO ()
main = runIO solve

