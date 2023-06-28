{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

-- | `vector`-based sparse graph implementation (weightened or unweightened).
--
-- Heavily inspired by @cojna/iota@.
module Data.Graph.Sparse where

import Control.Monad
import Control.Monad.ST
import Data.Graph (Vertex)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

type Edge = (Vertex, Vertex)

-- | Weightened edge
type WEdgeWith w = (Vertex, Vertex, w)

type EdgeId = Int

-- | CSR (compressed sparse row) representation of a graph (weightened or unweightened)
data SparseGraph w = SparseGraph
  { -- | Number of vertices.
    nVertsSG :: !Int,
    -- | Number of edges.
    nEdgesSG :: !Int,
    -- | Number of edges.
    offsetsSG :: !(VU.Vector Int),
    -- | Adjacent vertices sorted with starting vertex.
    adjacentsSG :: !(VU.Vector Vertex),
    -- | Edge weight information.
    edgeWeightsSG :: !(VU.Vector w)
  }
  deriving (Show)

-- | Builds a unweightned `SparseGraph`.
--
-- TODO: Faster implementation
{-# INLINE buildUSG #-}
buildUSG :: Int -> Int -> [(Int, Int)] -> SparseGraph Int
buildUSG nVertsSG nEdgesSG edges =
  buildWSG nVertsSG nEdgesSG $ map (\(!v1, !v2) -> (v1, v2, 1)) edges

-- | Builds a weightned `SparseGraph`.
{-# INLINE buildWSG #-}
buildWSG :: Int -> Int -> [(Int, Int, Int)] -> SparseGraph Int
buildWSG nVertsSG nEdgesSG edges =
  let !offsetsSG = (VU.prescanl' (+) 0) $ VU.create $ do
        !outDegs <- VUM.replicate nEdgesSG (0 :: Int)
        forM_ edges $ \(!v1, !_, !_) -> do
          VUM.modify outDegs succ v1
        return outDegs

      (!adjacentsSG, !edgeWeightsSG) = runST $ do
        !mOffsets <- VU.thaw offsetsSG
        !mAdjacents <- VUM.unsafeNew nEdgesSG
        !mWeights <- VUM.unsafeNew nEdgesSG

        forM_ edges $ \(!v1, !v2, !w) -> do
          !iEdgeFlatten <- VUM.unsafeRead mOffsets v1
          VUM.unsafeWrite mOffsets v1 (iEdgeFlatten + 1)
          VUM.unsafeWrite mAdjacents iEdgeFlatten v2
          VUM.unsafeWrite mWeights iEdgeFlatten w

        (,) <$> VU.unsafeFreeze mAdjacents <*> VU.unsafeFreeze mWeights
   in SparseGraph {..}
