{-# LANGUAGE CPP #-}
#include "./__import"
-- {{{ toy-lib import
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB
import Data.Instances.Affine2d
import Data.ModInt
import Data.UnionFind.Potencial
import Data.Vector.IxVector
-- }}} toy-lib import

{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug :: Bool
debug = True

{- ORMOLU_DISABLE -}
type MyModulo = (998244353 :: Nat) -- (1_000_000_007 :: Nat)
type MyModInt = ModInt MyModulo ; myMod :: Int ; myMod = fromInteger $ natVal' @MyModulo proxy# ; {-# INLINE modInt #-} ; modInt :: Int -> MyModInt ; modInt = ModInt . (`rem` myMod) ;
{- ORMOLU_ENABLE -}

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !q) <- ints2'
  qs <- U.replicateM q $ int' >>= \case
    0 -> (0 :: Int,,,) <$> int' <*> int' <*> int'
    1 -> (1 :: Int,,, -1 :: Int) <$> int' <*> int'
    _ -> error "unreachable"
  uf <- newPUF n
  res <- (`U.mapM` qs) $ \case
    (0, !u, !v, Sum . modInt -> !dx) -> do
      b <- canUnifyPUF uf u v dx
      unifyPUF_ uf u v dx
      return $ bool 0 1 b
    (1, !u, !v, !_) -> do
      maybe (-1 :: Int) (unModInt . getSum) <$> diffMayPUF uf u v
    _ -> error "unreachable"
  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/unionfind_with_potential
-- #potencial-union-find
main :: IO ()
main = runIO solve
