{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Minimum cost flow calculation (Ford-Fulkerson algorithm).
--
-- = Typical problems
--
-- - [min_cost_b_flow](https://judge.yosupo.jp/problem/min_cost_b_flow)
module Data.Graph.MinCostFlow where

-- TODO: unify Cost and Capacity with c
-- TODO: maybe use the same structure with Dinic
-- - SparseGraph + CSR
-- - addEdge
-- - etc.

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST
import Data.Graph.Alias (EdgeId, Vertex)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- TODO: use type parameter?
type Cost = Int

-- | The state for MinCostFlow
--
-- = Internals
-- Internally the graph is stored in a CSR (compressed sparse row).
data MinCostFlow s cap = MinCostFlow
  { nVertsMCF :: !Int,
    nEdgesMCF :: !Int,
    -- | Source vertex -> initial edge index. Note that edges are sourted by the starting vertex.
    offsetMCF :: !(U.Vector Int),
    -- | Edge index -> destination vertex.
    edgeDstMCF :: !(U.Vector Int),
    -- | Edge index -> reverse edge index.
    edgeRevIndexMCF :: !(U.Vector Int),
    -- | Edge index -> residual edge capacity.
    edgeCapMCF :: !(UM.MVector s cap),
    -- | Edge index -> cost
    edgeCostMCF :: !(U.Vector Cost)
  }

-- | Mutable states on running MinCostFlow algorithm.
data MinCostFlowBuffer s cap = MinCostFlowBuffer
  { -- | Shortest distance search result.
    distsMCF :: !(UM.MVector s Int),
    -- | Shortest distance + last vertex
    prevVertMCF :: !(UM.MVector s Vertex),
    -- | Shortest distance + last edge
    prevEdgeMCF :: !(UM.MVector s EdgeId)
  }

-- | Returns the minimum cost to for getting the flow, or Nothing when impossible.
minCostFlow :: (U.Unbox cap, Num cap, Integral cap, Ord cap, Bounded cap) => Int -> Int -> Int -> cap -> U.Vector (Vertex, Vertex, cap, Cost) -> Maybe Cost
minCostFlow !nVerts !src !sink !targetFlow !edges = runST $ do
  !container <- buildMinCostFlow nVerts edges
  runMinCostFlow src sink targetFlow container

undefMCF :: Int
undefMCF = -1

-- | Builds `MinCostFlow` from edges.
buildMinCostFlow ::
  forall cap m.
  (U.Unbox cap, Num cap, PrimMonad m) =>
  Int ->
  U.Vector (Vertex, Vertex, cap, Cost) ->
  m (MinCostFlow (PrimState m) cap)
buildMinCostFlow !nVertsMCF !edges = do
  let !offsetMCF = U.scanl' (+) (0 :: Int) $ U.create $ do
        !degs <- UM.replicate nVertsMCF (0 :: Int)
        G.forM_ edges $ \(!v1, !v2, !_, !_) -> do
          GM.modify degs (+ 1) v1
          GM.modify degs (+ 1) v2
        return degs

  (!edgeDstMCF, !edgeRevIndexMCF, !edgeCostMCF, !edgeCapMCF) <- do
    -- TODO: use unsafeNew
    !edgeDst <- UM.replicate nEdgesMCF undefMCF
    !edgeRevIndex <- UM.replicate nEdgesMCF undefMCF
    !edgeCost <- UM.replicate nEdgesMCF (0 :: Cost)
    !edgeCap <- UM.replicate nEdgesMCF (0 :: cap)

    -- add the edges and the reverse edges
    !edgeCounter <- U.thaw offsetMCF
    G.forM_ edges $ \(!v1, !v2, !cap, !cost) -> do
      -- consume the edge index
      !i1 <- GM.read edgeCounter v1
      !i2 <- GM.read edgeCounter v2
      GM.modify edgeCounter (+ 1) v1
      GM.modify edgeCounter (+ 1) v2
      -- store the edge
      GM.write edgeRevIndex i1 i2
      GM.write edgeRevIndex i2 i1
      GM.write edgeDst i1 v2
      GM.write edgeDst i2 v1
      GM.write edgeCap i1 cap
      -- GM.write edgeCap i2 0
      GM.write edgeCost i1 cost
      GM.write edgeCost i2 (-cost)

    (,,,edgeCap) <$> G.unsafeFreeze edgeDst <*> G.unsafeFreeze edgeRevIndex <*> G.unsafeFreeze edgeCost

  return MinCostFlow {..}
  where
    -- be sure to consider reverse edges
    !nEdgesMCF = G.length edges * 2

-- TODO: Does `Bounded` work for `Double` for example?

-- | Runs the MinCostFlow algorithm.
runMinCostFlow ::
  forall cap m.
  (U.Unbox cap, Integral cap, Bounded cap, PrimMonad m) =>
  Vertex ->
  Vertex ->
  cap ->
  MinCostFlow (PrimState m) cap ->
  m (Maybe Cost)
runMinCostFlow !src !sink !targetFlow dinic@MinCostFlow {..} = do
  bufs@MinCostFlowBuffer {..} <-
    -- distsMCF, prevVertMCF, prevEdgeMCF
    MinCostFlowBuffer
      <$> UM.unsafeNew nVertsMCF
      <*> UM.unsafeNew nVertsMCF
      <*> UM.unsafeNew nVertsMCF

  let run (!accCost :: Cost) !restFlow
        -- TODO: Is it ok to flow too much?
        | restFlow <= 0 = return $ Just accCost
        | otherwise = do
            -- clear buffers
            GM.set distsMCF undefMCF
            GM.set prevVertMCF undefMCF
            GM.set prevEdgeMCF undefMCF

            -- get the shortest path
            runMinCostFlowShortests src dinic bufs

            !distSink <- UM.read distsMCF sink
            if distSink == maxBound
              then return Nothing
              else do
                -- go back to the source from the sink, collection the shortest capacity
                !df <- flip fix (targetFlow, sink) $ \loop (!flow2, !v2) -> do
                  if v2 == src
                    then return flow2
                    else do
                      !v1 <- UM.read prevVertMCF v2
                      !i12 <- UM.read prevEdgeMCF v2
                      !flow1 <- UM.read edgeCapMCF i12
                      loop (min flow2 flow1, v1)

                -- FIXME: Cost and Capacity must be of the same type, so use `c` for both of them.
                let accCost' = accCost + (fromIntegral (toInteger df) :: Int) * distSink
                let restFlow' = restFlow - df

                -- go back to the source from the sink, modifying the capacities
                flip fix sink $ \loop v2 -> do
                  if v2 == src
                    then return ()
                    else do
                      !v1 <- UM.read prevVertMCF v2

                      !i12 <- UM.read prevEdgeMCF v2
                      UM.modify edgeCapMCF (subtract df) i12
                      let !i21 = edgeRevIndexMCF U.! v1
                      UM.modify edgeCapMCF (+ df) i21

                      loop v1

                run accCost' restFlow'

  run (0 :: Cost) targetFlow

-- | Collect shortest distances from the source vertex using Bellman-Ford algorithm.
--
-- TODO: break?
-- TODO: replace with dijkstra with potencials.
runMinCostFlowShortests ::
  forall c m.
  (U.Unbox c, Num c, Ord c, Bounded c, PrimMonad m) =>
  Vertex ->
  MinCostFlow (PrimState m) c ->
  MinCostFlowBuffer (PrimState m) c ->
  m ()
runMinCostFlowShortests !src MinCostFlow {..} MinCostFlowBuffer {..} = do
  UM.write distsMCF src 0

  fix $ \loop -> do
    !anyUpdate <- (\f -> U.foldM' f False (U.generate nVertsMCF id)) $ \anyUpdate v1 -> do
      !d1 <- UM.read distsMCF v1
      if d1 == undefMCF
        then do
          -- unreachable
          return anyUpdate
        else do
          let !iStart = offsetMCF U.! v1
              !iEnd = offsetMCF U.! (v1 + 1)
          (\f -> U.foldM' f False (U.generate (iEnd - iStart) (+ iStart))) $ \anyUpdate i12 -> do
            let !v2 = edgeDstMCF U.! i12
            let !cost12 = edgeCostMCF U.! i12
            !d2 <- UM.read distsMCF v2
            let d2' = d1 + cost12
            when (d2' < d2) $ do
              UM.write distsMCF v2 d2'
              UM.write prevVertMCF v2 v1
              UM.write prevEdgeMCF v2 i12
            return $! anyUpdate || d2' < d2
    when anyUpdate $ do
      loop
