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
import Data.Graph.Tree.Lca
import Data.Ix
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

----------------------------------------------------------------------------------------------------
-- LCA
----------------------------------------------------------------------------------------------------

-- | \(O(N)\) Returns @(depths, parents)@.
treeDepthInfoSG :: (Monoid w, U.Unbox w) => SparseGraph Int w -> Int -> (U.Vector Int, TransitionalSemigroup w)
treeDepthInfoSG gr@SparseGraph {..} !root = runST $ do
  !parents <- UM.unsafeNew nVerts
  !depths <- UM.unsafeNew nVerts

  flip fix (0 :: Int, -1 :: Int, U.singleton (root, mempty)) $ \loop (!depth, !parent, !vs) -> do
    U.forM_ vs $ \(!v, !w) -> do
      UM.unsafeWrite depths v depth
      UM.unsafeWrite parents v (parent, w)
      let !vs' = U.filter ((/= parent) . fst) $ gr `adjW` v
      loop (depth + 1, v, vs')

  (,) <$> U.unsafeFreeze depths <*> (TransitionalSemigroup <$> U.unsafeFreeze parents)
  where
    !nVerts = rangeSize boundsSG

-- | Returns `LcaCache`, i.e., @(parents, depths, parents')@.
lcaCacheSG :: (Monoid w, U.Unbox w) => SparseGraph Int w -> Vertex -> LcaCache w
lcaCacheSG !gr !root = (depths, toParent, cacheBL toParent)
  where
    (!depths, !toParent) = treeDepthInfoSG gr root

----------------------------------------------------------------------------------------------------
-- Tree folding
----------------------------------------------------------------------------------------------------

-- TODO: consider to not require semigroup aciton?
foldTreeImpl :: forall m op a w. (Monad m) => SparseGraph Int w -> Vertex -> (op -> a -> a) -> (Vertex -> a) -> (a -> op) -> (Vertex -> a -> m ()) -> m a
foldTreeImpl !tree !root !sact_ !acc0At !toOp !memo = inner (-1) root
  where
    inner :: Vertex -> Vertex -> m a
    inner !parent !v1 = do
      let !acc0 = acc0At v1
      !res <- U.foldM' (\acc v2 -> (`sact_` acc) . toOp <$> inner v1 v2) acc0 v2s
      memo v1 res
      return res
      where
        !v2s = U.filter (/= parent) $ tree `adj` v1

-- | \(O(N)\) Folds a tree from one root vertex using postorder DFS.
foldTree :: SparseGraph Int w -> Vertex -> (op -> a -> a) -> (Vertex -> a) -> (a -> op) -> a
foldTree !tree !root !sact_ !acc0At !toOp = runIdentity $ foldTreeImpl tree root sact_ acc0At toOp (\_ _ -> return ())

-- | \(O(N)\) Folds a tree from one root vertex using postorder DFS, recording all the accumulation values
-- on every vertex.
scanTree :: (G.Vector v a) => SparseGraph Int w -> Vertex -> (op -> a -> a) -> (Vertex -> a) -> (a -> op) -> v a
scanTree !tree !root !sact_ !acc0At !toOp = G.create $ do
  dp <- GM.unsafeNew nVerts
  !_ <- foldTreeImpl tree root sact_ acc0At toOp $ \v a -> do
    GM.unsafeWrite dp v a
  return dp
  where
    !nVerts = rangeSize $! boundsSG tree

-- | \(O(N)\) Type-restricted `scanTree`.
scanTreeU :: (U.Unbox a) => SparseGraph Int w -> Vertex -> (op -> a -> a) -> (Vertex -> a) -> (a -> op) -> U.Vector a
scanTreeU = scanTree

-- | \(O(N)\) Type-restricted `scanTree`.
scanTreeV :: SparseGraph Int w -> Vertex -> (op -> a -> a) -> (Vertex -> a) -> (a -> op) -> V.Vector a
scanTreeV = scanTree

-- | \(O(N)\) Folds a tree for every vertex as a root using the rerooting technique.
-- REMARK: `mempty` is used for initial operator value.
--
-- = Typical problems
-- - [Typical 039 - Tree Distance (★5)](https://atcoder.jp/contests/typical90/tasks/typical90_am)
-- - [EDPC V - Subtree](https://atcoder.jp/contests/dp/tasks/dp_v)
-- - [TDPC N - tree](https://atcoder.jp/contests/tdpc/tasks/tdpc_tree)
foldTreeAllSG :: forall a op w. (U.Unbox a, U.Unbox op, MonoidAction op a) => SparseGraph Int w -> (Vertex -> a) -> (a -> op) -> U.Vector a
foldTreeAllSG !tree !acc0At !toOp =
  -- Calculate tree DP for one root vertex
  let !treeDp = scanTreeU tree root0 mact acc0At toOp
      !rootDp = U.create $ do
        -- Calculate tree DP for every vertex as a root:
        !dp <- UM.unsafeNew nVerts
        let reroot parent parentOp v1 = do
              let !children = U.filter (/= parent) $ tree `adj` v1
              let !opL = U.scanl' (\op v2 -> (op <>) . toOp $ treeDp U.! v2) op0 children
              let !opR = U.scanr' (\v2 op -> (<> op) . toOp $ treeDp U.! v2) op0 children

              -- save
              let !x1 = (parentOp <> U.last opL) `mact` acc0At v1
              UM.unsafeWrite dp v1 x1

              U.iforM_ children $ \i2 v2 -> do
                let !lrOp = (opL U.! i2) <> (opR U.! succ i2)
                let !v1Acc = (parentOp <> lrOp) `mact` acc0At v2
                let !op' = toOp v1Acc
                reroot v1 op' v2

        reroot (-1 :: Vertex) op0 root0
        return dp
   in rootDp
  where
    !nVerts = rangeSize $ boundsSG tree
    !root0 = 0 :: Int
    !op0 = mempty @op
