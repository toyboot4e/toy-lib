{-# LANGUAGE CPP #-}
#include "./__import"
-- {{{ toy-lib import

import Data.Core.SemigroupAction
import Data.Graph.Tree.LCT
import Data.ModInt
import Data.Instances.Affine1
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
  (!n, !q) <- ints2'
  abs <- U.replicateM n ints2'
  uvs <- U.replicateM (n - 1) ints2'
  qs <-
    U.replicateM q $
      int' >>= \case
        0 -> (0 :: Int,,,,) <$> int' <*> int' <*> int' <*> int'
        1 -> (1,,,,-1) <$> int' <*> int' <*> int'
        2 -> (2,,,,-1) <$> int' <*> int' <*> int'
        _ -> error "unreachable"

  lct <- buildLCT (U.map (Affine1 . both modInt) abs) uvs
  res <- (`U.mapMaybeM` qs) $ \case
    (0, !u, !v, !w, !x) -> do
      -- delete edge (u, v)
      cutLCT lct u v
      -- add edge (w, x)
      linkLCT lct w x
      return Nothing
    (1, !p, !c, !d, !_) -> do
      -- set f(x) := cx + d
      writeLCT lct p $ Affine1 (modInt c, modInt d)
      return Nothing
    (2, !u, !v, !x, !_) -> do
      -- Be sure to composite from @v@ to @u@ so that @f(u)@ is applied first:
      res <- foldPathLCT lct v u
      return $ Just $! res `sact` modInt x
    _ -> error "unreachable"

  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/dynamic_tree_vertex_set_path_composite
-- #lct
main :: IO ()
main = runIO solve
