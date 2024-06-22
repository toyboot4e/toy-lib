{-# LANGUAGE CPP #-}
#include "./__import"

-- {{{ toy-lib import

import Data.Core.SemigroupAction
import Data.Instances.Mat2x2
import Data.ModInt
import Data.SegmentTree.Lazy
import Math.NTT
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
  !xs0 <- intsU'
  !qs <- U.replicateM q $ (`fmap` ints') $ \case
    [!x0, !x1, !x2] -> (x0, x1, x2, -1, -1)
    [!x0, !x1, !x2, !x3, !x4] -> (x0, x1, x2, x3, x4)
    _ -> error "unreachable"

  !stree <- generateLSTree n (\i -> V2 (modInt (xs0 U.! i), 1))

  G.forM_ qs $ \case
    (0, !l, !r, !a, !b) -> do
      sactLSTree stree l (r - 1) (Mat2x2 (modInt a, modInt b, 0, 1))
    (1, !l, !r, -1, -1) -> do
      V2 (!x, !_) <- foldLSTree stree l (r - 1)
      printBSB x
    _ -> error "unreachable"

-- verification-helper: PROBLEM https://atcoder.jp/contests/practice2/tasks/practice2_k
-- #lazy-segment-tree
main :: IO ()
main = runIO solve
