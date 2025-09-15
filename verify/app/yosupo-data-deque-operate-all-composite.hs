{-# LANGUAGE CPP #-}
#include "./__import"

import AtCoder.ModInt qualified as MI
-- {{{ toy-lib import

import Data.Core.SemigroupAction
import Data.Instances.Affine1
import Data.Slide
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB

-- }}} toy-lib import

modInt :: Int -> MI.ModInt 998244353
modInt = MI.new

{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug :: Bool
debug = False

solve :: StateT BS.ByteString IO ()
solve = do
  q <- int'
  qs <- U.replicateM q $ do
    int' >>= \case
      0 -> (0 :: Int,,) <$> int' <*> int'
      1 -> (1 :: Int,,) <$> int' <*> int'
      2 -> return (2 :: Int, -1, -1)
      3 -> return (3 :: Int, -1, -1)
      4 -> (4 :: Int,,-1) <$> int'
      _ -> error "unreachable"

  window <- newDSF q
  res <- (`U.mapMaybeM` qs) $ \case
    (0, !a, !b) -> do
      -- push front
      pushFrontDSF window . Dual $ Affine1 (MI.modInt a, MI.modInt b)
      return Nothing
    (1, !a, !b) -> do
      -- push back
      pushBackDSF window . Dual $ Affine1 (MI.modInt a, MI.modInt b)
      return Nothing
    (2, !_, !_) -> do
      -- pop froot
      popFrontDSF window
      return Nothing
    (3, !_, !_) -> do
      -- pop back
      popBackDSF window
      return Nothing
    (4, !x, !_) -> do
      -- fold
      Dual affine <- foldDSF window
      return . Just $ affine `sact` modInt x
    _ -> error "unreachable"

  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/deque_operate_all_composite
-- #sliding-fold
main :: IO ()
main = runIO solve
