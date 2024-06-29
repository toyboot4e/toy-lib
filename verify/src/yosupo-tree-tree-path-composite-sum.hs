{-# LANGUAGE CPP #-}
#include "./__import"

-- {{{ toy-lib import

import Data.Core.SemigroupAction
import Data.Graph.Alias
import Data.Graph.Sparse
-- import Data.Graph.Tree.TreeSG
import Data.Instances.Affine2d
import Data.ModInt
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
  n <- int'
  xs <- U.map (toV2 . modInt) <$> intsU'
  es <- U.replicateM (n - 1) $ do
    (\(!u, !v, !a, !b) -> (u, v, Affine2d (modInt a, modInt b))) <$> ints4'

  let !gr = buildWSG n $ swapDupeW es

  -- both `Acc` and `Op` are `V2`. only the edge weights are `Affine2d`.
  let res = foldTreeAllSG' gr onEdge acc0At toOp
        where
          onEdge !op (!_, !affine) = affine `sact` op
          acc0At v = xs G.! v
          toOp = id

  printVec $ U.map unV2 res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/tree_path_composite_sum
-- #rerooting
main :: IO ()
main = runIO solve
