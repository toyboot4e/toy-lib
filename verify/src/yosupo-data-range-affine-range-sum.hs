{-# LANGUAGE CPP #-}
#include "./__import"

-- {{{ toy-lib import

import Data.Instances.Affine2d
import Data.ModInt
import Data.SegmentTree.Lazy
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB

-- }}} toy-lib import

import Data.Functor.Identity

{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug = False

{- ORMOLU_DISABLE -}
type MyModulo = (998244353 :: Nat) -- (1_000_000_007 :: Nat)
type MyModInt = ModInt MyModulo ; myMod :: Int ; myMod = fromInteger $ natVal' @MyModulo proxy# ; {-# INLINE modInt #-} ; modInt :: Int -> MyModInt ; modInt = ModInt . (`rem` myMod) ;
{- ORMOLU_ENABLE -}

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !q) <- ints2'
  xs <- intsU'
  qs <- U.replicateM q $ do
    int' >>= \case
      0 -> (0 :: Int,,,,) <$> int' <*> int' <*> int' <*> int'
      1 -> (1,,,-1,-1) <$> int' <*> int'

  stree <- buildLSTree $ U.map (V1 . Sum . modInt) xs

  res <- (`U.mapMaybeM` qs) $ \case
    (0, !l, pred -> !r, !a, !b) -> do
      sactLSTree stree l r $ Affine2d (Sum (modInt a), Sum (modInt b))
      return Nothing
    (1, !l, pred -> !r, !_, !_) -> do
      V1 (Sum x) <- foldLSTree stree l r
      return $ Just x
    _ -> error "unreachable"

  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/range_affine_range_sum
-- #segment-tree
main :: IO ()
main = runIO solve
