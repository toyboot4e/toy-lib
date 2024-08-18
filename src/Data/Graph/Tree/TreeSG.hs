{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Tree algorithms for `SparseGraph`.
module Data.Graph.Tree.TreeSG where

import Control.Monad.Fix
import Control.Monad.ST
import Data.BinaryLifting
import Data.Core.SemigroupAction
import Data.Functor.Identity
import Data.Graph.Alias (Vertex)
import Data.Graph.Sparse
import Data.Graph.Generic (restorePath)
import Data.Graph.Tree.Lca
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- * Diameter

-- | \(O(V+E)\) Tree diameter calculation.
treeDiameterSG :: (U.Unbox w, Num w, Ord w) => SparseGraph w -> w -> (Vertex, Vertex, w)
treeDiameterSG gr undefW =
  let !dfs0 = bfsSG gr 0 undefW
      !from = U.maxIndex dfs0
      !dfs1 = bfsSG gr from undefW
      !to = U.maxIndex dfs1
      !w = U.maximum dfs1
   in (from, to, w)

-- | \(O(V+E)\) Tree diameter calculation.
treeDiameterPathSG :: (U.Unbox w, Num w, Ord w) => SparseGraph w -> w -> ((Vertex, Vertex, w), U.Vector Vertex)
treeDiameterPathSG gr undefW =
  let !bfs0 = bfsSG gr 0 undefW
      !from = U.maxIndex bfs0
      (!bfs1, !parents) = bfsTreeSG gr from undefW
      !to = U.maxIndex bfs1
      !w = bfs1 U.! to
   in ((from, to, w), restorePath parents to)

-- TODO: height

-- * LCA (prefer HLD though)

-- | \(O(N)\) Returns @(depths, parents)@.
treeDepthInfoSG :: (Monoid w, U.Unbox w) => SparseGraph w -> Int -> (U.Vector Int, IndexMapWithAction w)
treeDepthInfoSG gr@SparseGraph {..} !root = runST $ do
  !parents <- UM.unsafeNew nVertsSG
  !depths <- UM.unsafeNew nVertsSG

  flip fix (0 :: Int, -1 :: Int, U.singleton (root, mempty)) $ \loop (!depth, !parent, !vs) -> do
    U.forM_ vs $ \(!v, !w) -> do
      UM.unsafeWrite depths v depth
      UM.unsafeWrite parents v (parent, w)
      let !vs' = U.filter ((/= parent) . fst) $ gr `adjW` v
      loop (depth + 1, v, vs')

  (,) <$> U.unsafeFreeze depths <*> (IndexMapWithAction <$> U.unsafeFreeze parents)

-- | Returns `LcaCache`, i.e., @(parents, depths, parents')@.
lcaCacheSG :: (Monoid w, U.Unbox w) => SparseGraph w -> Vertex -> LcaCache w
lcaCacheSG !gr !root = (depths, toParent, cacheBLV toParent)
  where
    (!depths, !toParent) = treeDepthInfoSG gr root

-- * Tree folding

-- | \(O(N)\) Weighted ree folding.
foldTreeImplSG :: forall m op a w. (Monad m, U.Unbox w) => SparseGraph w -> Vertex -> (op -> (Vertex, w) -> op) -> (op -> a -> a) -> (Vertex -> a) -> (a -> op) -> (Vertex -> a -> m ()) -> m a
foldTreeImplSG !tree !root !onEdge !sact_ !acc0At !toOp !memo = inner (-1) root
  where
    inner :: Vertex -> Vertex -> m a
    inner !parent !v1 = do
      let !acc0 = acc0At v1
      let !v2s = U.filter ((/= parent) . fst) $ tree `adjW` v1
      !res <- U.foldM' (\acc (!v2, !w) -> (`sact_` acc) . (`onEdge` (v1, w)) . toOp <$> inner v1 v2) acc0 v2s
      memo v1 res
      return res

-- | \(O(N)\) Folds a tree from one root vertex using postorder DFS.
foldTreeSG :: (U.Unbox w) => SparseGraph w -> Vertex -> (op -> (Vertex, w) -> op) -> (op -> a -> a) -> (Vertex -> a) -> (a -> op) -> a
foldTreeSG !tree !root !onEdge !sact_ !acc0At !toOp = runIdentity $ do
  foldTreeImplSG tree root onEdge sact_ acc0At toOp (\_ _ -> return ())

-- | \(O(N)\) Folds a tree from one root vertex using postorder DFS, recording all the accumulation values
-- on every vertex.
scanTreeSG :: (G.Vector v a, U.Unbox w) => SparseGraph w -> Vertex -> (op -> (Vertex, w) -> op) -> (op -> a -> a) -> (Vertex -> a) -> (a -> op) -> v a
scanTreeSG !tree !root !onEdge !sact_ !acc0At !toOp = G.create $ do
  dp <- GM.unsafeNew (nVertsSG tree)
  !_ <- foldTreeImplSG tree root onEdge sact_ acc0At toOp $ \v a -> do
    GM.unsafeWrite dp v a
  return dp

-- | \(O(N)\) Folds a not-weighted tree for every vertex as a root using the rerooting technique.
--
-- = Typical problems
-- - [Typical 039 - Tree Distance (★5)](https://atcoder.jp/contests/typical90/tasks/typical90_am)
-- - [EDPC V - Subtree](https://atcoder.jp/contests/dp/tasks/dp_v)
-- - [TDPC N - tree](https://atcoder.jp/contests/tdpc/tasks/tdpc_tree)
foldTreeAllSG :: forall a op w. (U.Unbox a, U.Unbox op, Monoid op, SemigroupAction op a, U.Unbox w) => SparseGraph w -> (Vertex -> a) -> (a -> op) -> U.Vector a
foldTreeAllSG !tree = foldTreeAllSG' tree const

-- | \(O(N)\) Folds a weighted tree for every vertex as a root using the rerooting technique.
--
-- = Typical problems
-- - [Tree Path Composite Sum](https://judge.yosupo.jp/problem/tree_path_composite_sum)
foldTreeAllSG' :: forall a op w. (U.Unbox a, U.Unbox op, Monoid op, SemigroupAction op a, U.Unbox w) => SparseGraph w -> (op -> (Vertex, w) -> op) -> (Vertex -> a) -> (a -> op) -> U.Vector a
foldTreeAllSG' !tree !onEdge !acc0At !toOp =
  -- Calculate tree DP for one root vertex
  let !treeDp = scanTreeSG tree root0 onEdge sact acc0At toOp :: U.Vector a
      !rootDp = U.create $ do
        -- Calculate tree DP for every vertex as a root:
        !dp <- UM.unsafeNew (nVertsSG tree)
        let reroot parent parentOp v1 = do
              -- TODO: when the operator is not commutative?
              let !children = U.filter ((/= parent) . fst) $ tree `adjW` v1
              let !opL = U.scanl' (\ !op (!v2, !w) -> (op <>) . (`onEdge` (v1, w)) . toOp $ treeDp G.! v2) op0 children
              let !opR = U.scanr' (\(!v2, !w) !op -> (<> op) . (`onEdge` (v1, w)) . toOp $ treeDp G.! v2) op0 children

              -- save
              let !x1 = (parentOp <> U.last opL) `sact` acc0At v1
              UM.unsafeWrite dp v1 x1

              U.iforM_ children $ \i2 (!v2, !w) -> do
                -- composited operator excluding @v2@:
                let !op1 = parentOp <> (opL G.! i2) <> (opR G.! succ i2)
                let !v1Acc = op1 `sact` acc0At v1
                let !op2 = onEdge (toOp v1Acc) (v2, w)
                reroot v1 op2 v2

        reroot (-1 :: Vertex) op0 root0

        return dp
   in rootDp
  where
    !root0 = 0 :: Int
    !op0 = mempty @op
