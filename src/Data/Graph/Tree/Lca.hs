-- | Least common ancestor and folding via that.
module Data.Graph.Tree.Lca where

import Algorithm.Bisect
import Data.BinaryLifting
import Data.Core.SemigroupAction
import Data.Graph.Alias (Vertex)
import Data.Maybe
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import GHC.Stack (HasCallStack)

-- | `(parents, depths, parents')`
type LcaCache a = (U.Vector Int, TransiteSemigroup a, VecBL (TransiteSemigroup a))

-- | Returns the lowest common ancestor `(v, d)` with the help of the binary lifting technique.
-- REMARK: Use 0-based index for the graph vertices.
lca :: (HasCallStack, U.Unbox a) => LcaCache a -> Vertex -> Vertex -> (Vertex, Int)
lca (!depths, !_, !toParentN) !v1 !v2 = (vLCA, depths U.! vLCA)
  where
    -- depths
    !d1 = depths U.! v1
    !d2 = depths U.! v2

    -- parentN n v -> p_{v, n}
    parentN = sactBL toParentN

    -- v1' and v2' are of the same depth:
    !v1' = if d1 <= d2 then v1 else v2
    !v2' = parentN (abs $ d1 - d2) (if d1 > d2 then v1 else v2)
    -- !_ = dbg ((v1, v2), (d1 ,d2), (v1', v2'))

    -- find the depth of the lowest common ancestor:
    !dLCA = fromJust . snd $ bisect 0 (min d1 d2) $ \d ->
      parentN d v1' /= parentN d v2'

    !vLCA = parentN dLCA v1'

-- | Gets the length between given two vertices with the help of LCA.
lcaLen :: (HasCallStack, U.Unbox a) => LcaCache a -> Int -> Int -> Int
lcaLen cache@(!depths, !_, !_) !v1 !v2 =
  let (!_, !d) = lca cache v1 v2
      !d1 = depths U.! v1
      !d2 = depths U.! v2
   in (d1 - d) + (d2 - d)

----------------------------------------------------------------------------------------------------
-- Tree path folding
----------------------------------------------------------------------------------------------------

-- | Returns `FoldLcaCache` for folding values between two vertices.
--
-- - @graph@: Vertex -> [Vertex]
-- - @edgeValueOf@: child -> parent -> m
foldLcaCache :: forall m. (Monoid m, U.Unbox m) => LcaCache m -> VecBL (TransiteSemigroup m)
foldLcaCache (!_, !toParent, !_) = cacheBL . TransiteSemigroup $ U.generate (G.length (unTransiteSemigroup toParent)) f
  where
    f c
      | p == -1 = (-1, mempty)
      | otherwise = (p, op)
      where
        (!p, !op) = toParent `sact` (c, mempty)

-- | Calculates the folding value of the path between two vertices in a tree.
--
-- = Typical problems
-- - [ABC 235 E - MST + 1](https://atcoder.jp/contests/abc235/tasks/abc235_e)
--   In this problems we have self-looping edge though.
foldViaLca :: forall a. (HasCallStack, U.Unbox a, Semigroup a) => LcaCache a -> (Vertex, a) -> (Vertex, a) -> a
foldViaLca cache@(!depths, !_, !toParentBL) (!v1, !a1) (!v2, !a2) = a'
  where
    (!_, !d) = lca cache v1 v2
    d1 = depths U.! v1
    d2 = depths U.! v2
    (!_, !a1') = sactBL toParentBL (d1 - d) (v1, a1)
    (!_, !a2') = sactBL toParentBL (d2 - d) (v2, a2)
    !a' = a1' <> a2'

