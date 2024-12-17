{-# LANGUAGE CPP #-}
#include "./__import"

-- {{{ toy-lib import

import Data.Instances.Affine1
import Data.ModInt
import Data.SegmentTree.Lazy
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB

-- }}} toy-lib import

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
      _ -> error "unreachable"

  -- Affine1
  !stree <- buildLSTree $ U.map (toV2 . modInt) xs
  res <- (`U.mapMaybeM` qs) $ \case
    (0, !l, pred -> !r, !a, !b) -> do
      sactLSTree stree l r $ Affine1 (modInt a, modInt b)
      return Nothing
    (1, !l, pred -> !r, !_, !_) -> do
      x <- unV2 <$> foldLSTree stree l r
      return $ Just x
    _ -> error "unreachable"

  -- Mat2x2
  -- stree <- buildLSTree $ U.map (\x -> V2 (modInt x, modInt 1)) xs
  -- res <- (`U.mapMaybeM` qs) $ \case
  --   (0, !l, pred -> !r, !a, !b) -> do
  --     sactLSTree stree l r $ Mat2x2 (modInt a, modInt b, modInt 0, modInt 1)
  --     return Nothing
  --   (1, !l, pred -> !r, !_, !_) -> do
  --     V2 (!x, !_) <- foldLSTree stree l r
  --     return $ Just x
  --   _ -> error "unreachable"

  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/range_affine_range_sum
-- #segment-tree #mat-2x2 #affine-2d
main :: IO ()
main = runIO solve
