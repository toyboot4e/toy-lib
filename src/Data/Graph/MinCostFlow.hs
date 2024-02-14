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
    offsetsMCF :: !(U.Vector Int),
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
  { -- | Vertex -> shortest distance search result.
    distsMCF :: !(UM.MVector s Int),
    -- | Vertex -> shortest distance + last vertex
    prevVertMCF :: !(UM.MVector s Vertex),
    -- | Vertex -> shortest distance + last edge
    prevEdgeMCF :: !(UM.MVector s EdgeId)
  }

-- | Returns the minimum cost to for getting the flow, or Nothing when impossible.
--
-- TODO: Return tuple just like ACL.
minCostFlow ::
  (Show cap, U.Unbox cap, Num cap, Integral cap, Ord cap, Bounded cap) =>
  Int ->
  Int ->
  Int ->
  cap ->
  U.Vector (Vertex, Vertex, cap, Cost) ->
  Maybe Cost
minCostFlow !nVerts !src !sink !targetFlow !edges = runST $ do
  fst <$> minCostFlow' nVerts src sink targetFlow edges

minCostFlow' ::
  (PrimMonad m, Show cap, U.Unbox cap, Num cap, Integral cap, Ord cap, Bounded cap) =>
  Int ->
  Int ->
  Int ->
  cap ->
  U.Vector (Vertex, Vertex, cap, Cost) ->
  m (Maybe Cost, MinCostFlow (PrimState m) cap)
minCostFlow' !nVerts !src !sink !targetFlow !edges = do
  !container <- buildMinCostFlow nVerts edges
  -- TODO: return max flow, too
  !minCost <- runMinCostFlow src sink targetFlow container
  return (minCost, container)

-- | Retrieves edge information @(v1, v2, cap, flow, cost)@ from the `maxFlow` results.
--
-- Be warned that it contains reverse edges and edge from/to source/sink.
edgesMCF :: (PrimMonad m, U.Unbox c, Num c, Ord c, Bounded c) => MinCostFlow (PrimState m) c -> m (U.Vector (Int, Int, c, c, Cost))
edgesMCF MinCostFlow {..} = do
  !edgeCap <- U.unsafeFreeze edgeCapMCF

  let next (!i12, !v1)
        | i12 == offsetsMCF U.! (v1 + 1) = next (i12, v1 + 1)
        | otherwise = ((v1, v2, cap, flow, cost), (i12 + 1, v1))
        where
          v2 = edgeDstMCF U.! i12
          i21 = edgeRevIndexMCF U.! i12
          flow = edgeCap U.! i21
          cap = edgeCap U.! i12 + edgeCap U.! i21
          cost = edgeCostMCF U.! i12

  return $ U.unfoldrExactN nEdgesMCF next ((0 :: Vertex), 0 :: Int)

-- | For vertices and edge indices?
undefMCF :: Int
undefMCF = -1

-- | Bellman-ford use
undefDistMCF :: Int
undefDistMCF = maxBound

-- | Builds `MinCostFlow` from edges.
buildMinCostFlow ::
  forall cap m.
  (Show cap, U.Unbox cap, Num cap, PrimMonad m) =>
  Int ->
  U.Vector (Vertex, Vertex, cap, Cost) ->
  m (MinCostFlow (PrimState m) cap)
buildMinCostFlow !nVertsMCF !edges = do
  let !offsetsMCF = U.scanl' (+) (0 :: Int) $ U.create $ do
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
    !edgeCounter <- U.thaw offsetsMCF
    G.forM_ edges $ \(!v1, !v2, !cap, !cost) -> do
      let !_ = dbgAssert (cost >= 0) $ "costs must be zero or positive" ++ show (v1, v2)
      let !_ = dbgAssert (v1 /= v2) $ "cannot use self loop edge: " ++ show (v1, v2)
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
  (Show cap, U.Unbox cap, Integral cap, Bounded cap, PrimMonad m) =>
  Vertex ->
  Vertex ->
  cap ->
  MinCostFlow (PrimState m) cap ->
  m (Maybe Cost)
runMinCostFlow !src !sink !targetFlow container@MinCostFlow {..} = do
  bufs@MinCostFlowBuffer {..} <-
    -- distsMCF, prevVertMCF, prevEdgeMCF
    MinCostFlowBuffer
      <$> UM.unsafeNew nVertsMCF
      <*> UM.unsafeNew nVertsMCF
      <*> UM.unsafeNew nVertsMCF

  let run !accCost !restFlow
        -- TODO: Is it ok to flow too much?
        | restFlow <= 0 =
            let !_ = dbgAssert (restFlow == 0) "flow too much?"
             in return $ Just accCost
        | otherwise = do
            -- clear buffers
            GM.set distsMCF undefDistMCF
            GM.set prevVertMCF undefMCF
            GM.set prevEdgeMCF undefMCF

            -- get the shortest path
            let !_ = dbg ("short")
            runMinCostFlowShortests src container bufs
            let !_ = dbg ("let's DFS")

            !distSink <- UM.read distsMCF sink
            if distSink == undefDistMCF
              then -- TODO: Is it ok to return less than the target flow?
                return Nothing
              else do
                let !_ = dbg ("flow", distSink)
                -- go back to the source from the sink, collection the shortest capacity
                !deltaFlow <- flip fix (restFlow, sink) $ \loop (!flow, !v2) -> do
                  if v2 == src
                    then return flow
                    else do
                      !v1 <- UM.read prevVertMCF v2
                      !i12 <- UM.read prevEdgeMCF v2
                      let !_ = dbg ("read", v1, v2, i12)
                      !cap12 <- UM.read edgeCapMCF i12
                      loop (min flow cap12, v1)

                let !_ = dbg (deltaFlow)
                let !_ = dbgAssert (deltaFlow >= 0) $ "negative delta flow?"

                -- FIXME: Cost and Capacity must be of the same type, so use `c` for both of them.
                let accCost' = accCost + (fromIntegral (toInteger deltaFlow) :: Int) * distSink
                let restFlow' = restFlow - deltaFlow

                -- go back to the source from the sink, modifying the capacities
                flip fix sink $ \loop v2 -> do
                  if v2 == src
                    then return ()
                    else do
                      !v1 <- UM.read prevVertMCF v2
                      !i12 <- UM.read prevEdgeMCF v2
                      let !i21 = edgeRevIndexMCF U.! i12
                      let !_ = dbg (v1, v2, i12, i21)
                      UM.modify edgeCapMCF (subtract deltaFlow) i12
                      UM.modify edgeCapMCF (+ deltaFlow) i21

                      -- FIXME: remove
                      !c1 <- UM.read edgeCapMCF i12
                      !c2 <- UM.read edgeCapMCF i21
                      let !_ = dbgAssert (c1 >= 0) $ "neg1"
                      let !_ = dbgAssert (c2 >= 0) $ "neg2"

                      loop v1

                let !_ = dbg (accCost', restFlow')
                run accCost' restFlow'

  run (0 :: Cost) targetFlow

-- | Collect shortest distances from the source vertex using Bellman-Ford algorithm.
--
-- Hangs on negative loop.
--
-- TODO: break?
-- TODO: replace with dijkstra with potencials.
runMinCostFlowShortests ::
  -- forall c m.
  (U.Unbox c, Num c, Ord c, Bounded c, PrimMonad m) =>
  Vertex ->
  MinCostFlow (PrimState m) c ->
  MinCostFlowBuffer (PrimState m) c ->
  m ()
runMinCostFlowShortests !src MinCostFlow {..} MinCostFlowBuffer {..} = do
  UM.write distsMCF src 0

  fix $ \loop -> do
    !b <- (\f -> U.foldM' f False (U.generate nVertsMCF id)) $ \ !anyUpdate v1 -> do
      !d1 <- UM.read distsMCF v1
      if d1 == undefDistMCF
        then do
          -- unreachable. skip
          return anyUpdate
        else do
          let !iStart = offsetsMCF U.! v1
              !iEnd = offsetsMCF U.! (v1 + 1)
          (\f -> U.foldM' f anyUpdate (U.generate (iEnd - iStart) (+ iStart))) $ \ !anyUpdate i12 -> do
            let !v2 = edgeDstMCF U.! i12
            !cap12 <- UM.read edgeCapMCF i12
            let !cost12 = edgeCostMCF U.! i12
            !d2 <- UM.read distsMCF v2
            let d2' = d1 + cost12
            -- let !_ = dbgAssert (d1 >= 0 && d2 >= 0) $ "negative distance?: " ++ show (d1, d2)
            if (cap12 > 0 && d2' < d2)
              then do
                let !_ = dbg ("update", (v1, v2), i12, d2, d2')
                UM.write distsMCF v2 d2'
                UM.write prevVertMCF v2 v1
                UM.write prevEdgeMCF v2 i12
                return $! anyUpdate || d2' < d2
              else do
                return anyUpdate

    when b $ do
      loop

