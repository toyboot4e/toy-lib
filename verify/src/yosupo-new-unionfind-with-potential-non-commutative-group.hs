{-# LANGUAGE CPP #-}
#include "./__import"
-- {{{ toy-lib import

import Data.Instances.Affine2d
import Data.ModInt
import Data.UnionFind.Potencial
import Data.Vector.IxVector
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB

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
  qs <- U.replicateM q $ do
    int' >>= \case
      0 -> (0 :: Int,,,) <$> int' <*> int' <*> ints4'
      1 -> (1 :: Int,,,(-1, -1, -1, -1)) <$> int' <*> int'
      _ -> error "unreachable"

  uf <- newPUF n
  res <- (`V.mapM` U.convert qs) $ \case
    (0, !u, !v, (!x00, !x01, !x10, !x11)) -> do
      let !dx = Dual $ Mat2x2 (modInt x00, modInt x01, modInt x10, modInt x11)
      b <- canUnifyPUF uf u v dx
      unifyPUF_ uf u v dx
      return . showBSB $ bool '0' '1' b
    (1, !u, !v, (!_, !_, !_, !_)) -> do
      maybe (showBSB "-1" ) (showBSB . unMat2x2 . getDual) <$> diffMayPUF uf u v
    _ -> error "unreachable"

  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/unionfind_with_potential_non_commutative_group
-- #potencial-union-find
main :: IO ()
main = runIO solve
