{-# LANGUAGE CPP #-}
#include "./__import"

-- {{{ toy-lib import

import Data.Core.SemigroupAction
import Data.Graph.Sparse
import Data.Graph.Tree.Hld
import Data.Instances.Affine1
import Data.ModInt
import Data.SegmentTree.Strict
import ToyLib.Debug
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB

-- }}} toy-lib import
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug = False

-- }}}

{- ORMOLU_DISABLE -}
type MyModulo = (998244353 :: Nat) -- (1_000_000_007 :: Nat)
type MyModInt = ModInt MyModulo ; myMod :: Int ; myMod = fromInteger $ natVal' @MyModulo proxy# ; {-# INLINE modInt #-} ; modInt :: Int -> MyModInt ; modInt = ModInt . (`rem` myMod) ;
{- ORMOLU_ENABLE -}

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !q) <- ints2'
  abs <- U.replicateM n ints2'
  uvs <- U.replicateM (n - 1) ints2'
  qs <- U.replicateM q ints4'

  let !gr = buildSG_ n $ swapDupeU uvs
  let !hld = hldOf gr
  !tm <- buildVertTM hld False $ U.map (\(!a, !b) -> Dual (Affine1 (modInt a, modInt b))) abs

  res <- (`U.mapMaybeM` qs) $ \case
    (0, !p, !c, !d) -> do
      writeTM tm p . Dual $ Affine1 (modInt c, modInt d)
      return Nothing
    (1, !u, !v, !x) -> do
      Dual !m <- foldTM tm u v
      return . Just . unV2 $ m `sact` toV2 (modInt x)
    _ -> error "unreachable"

  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/vertex_set_path_composite
-- #hld #affine
main :: IO ()
main = runIO solve
