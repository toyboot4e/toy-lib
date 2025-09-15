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

debug :: Bool
debug = False

{- ORMOLU_DISABLE -}
type MyModulo = (998244353 :: Nat) -- (1_000_000_007 :: Nat)
type MyModInt = ModInt MyModulo ; myMod :: Int ; myMod = fromInteger $ natVal' @MyModulo proxy# ; {-# INLINE modInt #-} ; modInt :: Int -> MyModInt ; modInt = ModInt . (`rem` myMod) ;
{- ORMOLU_ENABLE -}

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !q) <- ints2'
  !xs <- intsU'
  !qs <- U.replicateM q $ do
    int' >>= \case
      0 -> (0 :: Int,,,,) <$> int' <*> int' <*> int' <*> int'
      1 -> (1,,,-1,-1) <$> int' <*> int'
      _ -> error "unreachable"

  -- it is super important to have `1` as the second element
  !stree <- buildLSTree $ U.map (toV2 . modInt) xs

  res <- (`U.mapMaybeM` qs) $ \case
    (0, !l, !r, !a, !b) -> do
      sactLSTree stree l (r - 1) $ Affine1 (modInt a, modInt b)
      return Nothing
    (1, !l, !r, -1, -1) -> do
      x <- unV2 <$> foldLSTree stree l (r - 1)
      return $ Just x
    _ -> error "unreachable"

  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://atcoder.jp/contests/practice2/tasks/practice2_k
-- #lazy-segment-tree
main :: IO ()
main = runIO solve
