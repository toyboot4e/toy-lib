{-# LANGUAGE CPP #-}
#include "./__import"

-- {{{ toy-lib import

import Data.Core.SemigroupAction
import Data.Instances.Affine1
import Data.ModInt
import Data.Slide
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB

-- }}} toy-lib import

{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug :: Bool
debug = False

{- ORMOLU_DISABLE -}
type MyModulo = (998244353 :: Nat) -- (1_000_000_007 :: Nat)
type MyModInt = ModInt MyModulo ; myMod :: Int ; myMod = fromInteger $ natVal' @MyModulo proxy# ; {-# INLINE modInt #-} ; modInt :: Int -> MyModInt ; modInt = ModInt . (`rem` myMod) ;
{- ORMOLU_ENABLE -}

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
      pushFrontDSF window . Dual $ Affine1 (modInt a, modInt b)
      return Nothing
    (1, !a, !b) -> do
      -- push back
      pushBackDSF window . Dual $ Affine1 (modInt a, modInt b)
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
