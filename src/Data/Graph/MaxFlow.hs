{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | \(O(V^2 E)\) Max flow algorithm (Dinic's algorithm). Heavily inspired by @cojna/iota@.
--
-- Just run `maxFlow` to get the result. `maxFlow'` returns result with context.
--
-- = Typical problems
--
-- - [ABC 205 - F](https://atcoder.jp/contests/abc205/tasks/abc205_f)
module Data.Graph.MaxFlow where

import Control.Monad
import Control.Monad.Extra (whenJustM)
import Control.Monad.Fix
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST
import Data.Buffer
import Data.Graph.Alias (Vertex)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- | Max flow calculation information and states.
--
-- = Internals
-- Internally the graph is stored in a CSR (compressed sparse row).
--
-- = Invariants
-- - Sum of the capacities of one edge and the reverse edge is zero.
data MaxFlow s c = MaxFlow
  { nVertsMF :: !Int,
    nEdgesMF :: !Int,
    -- | Source vertex -> initial edge index. Note that edges are sourted by the starting vertex.
    offsetsMF :: !(U.Vector Int),
    -- | Edge index -> destination vertex.
    edgeDstMF :: !(U.Vector Int),
    -- | Edge index -> reverse edge index.
    edgeRevIndexMF :: !(U.Vector Int),
    -- | Edge index -> residual edge capacity.
    edgeCapMF :: !(UM.MVector s c)
  }

-- | Mutable states on running Dinic's algorithm.
data MaxFlowBuffer s c = MaxFlowBuffer
  { -- | BFS result.
    distsMF :: !(UM.MVector s Int),
    -- | Queue for BFS
    queueMF :: !(Buffer s Vertex),
    -- | DFS state for iterating through all the neighbors.
    iterMF :: !(UM.MVector s Int)
  }

-- | \(O(V^2E)\) max flow algorithm (Dinic's algorithm).
maxFlow ::
  (U.Unbox c, Num c, Ord c, Bounded c) =>
  Int ->
  Int ->
  Int ->
  U.Vector (Vertex, Vertex, c) ->
  c
maxFlow !nVerts !src !sink !edges = runST $ do
  fst <$> maxFlow' nVerts src sink edges

-- | Runs `maxFlow` and retrieves the last state.
--
-- TODO: Return a freezed version?
maxFlow' ::
  (PrimMonad m, U.Unbox c, Num c, Ord c, Bounded c) =>
  Int ->
  Int ->
  Int ->
  U.Vector (Vertex, Vertex, c) ->
  m (c, MaxFlow (PrimState m) c)
maxFlow' !nVerts !src !sink !edges = do
  !container <- buildMaxFlow nVerts edges
  !flow <- runMaxFlow src sink container
  return (flow, container)

-- | Handy API for retrieving edge information @(v1, v2, cap, flow)@ from the `maxFlow` results.
--
-- Be warned that it contains reverse edges and edge from/to source/sink.
edgesMF :: (PrimMonad m, U.Unbox c, Num c) => MaxFlow (PrimState m) c -> m (U.Vector (Int, Int, c, c))
edgesMF MaxFlow {..} = do
  !edgeCap <- U.unsafeFreeze edgeCapMF

  let next (!i12, !v1)
        | i12 == offsetsMF G.! (v1 + 1) = next (i12, v1 + 1)
        | otherwise = ((v1, v2, cap, flow), (i12 + 1, v1))
        where
          v2 = edgeDstMF G.! i12
          i21 = edgeRevIndexMF G.! i12
          flow = edgeCap G.! i21
          cap = edgeCap G.! i12 + edgeCap G.! i21

  return $ U.unfoldrExactN nEdgesMF next (0 :: Vertex, 0 :: Int)

undefMF :: Int
undefMF = -1

-- | Builds `MaxFlow` from edges.
buildMaxFlow ::
  forall c m.
  (U.Unbox c, Num c, PrimMonad m) =>
  Int ->
  U.Vector (Vertex, Vertex, c) ->
  m (MaxFlow (PrimState m) c)
buildMaxFlow !nVertsMF !edges = do
  let !offsetsMF = U.scanl' (+) (0 :: Int) $ U.create $ do
        !degs <- UM.replicate nVertsMF (0 :: Int)
        G.forM_ edges $ \(!v1, !v2, !_) -> do
          GM.modify degs (+ 1) v1
          GM.modify degs (+ 1) v2
        return degs

  (!edgeDstMF, !edgeRevIndexMF, !edgeCapMF) <- do
    !edgeDst <- UM.unsafeNew nEdgesMF
    !edgeRevIndex <- UM.unsafeNew nEdgesMF
    !edgeCap <- UM.unsafeNew nEdgesMF

    -- add the edges and the reverse edges
    -- TODO: reuse it as `iterMF`
    !edgeCounter <- U.thaw offsetsMF
    G.forM_ edges $ \(!v1, !v2, !cap) -> do
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
      GM.write edgeCap i2 (0 :: c)

    (,,edgeCap) <$> G.unsafeFreeze edgeDst <*> G.unsafeFreeze edgeRevIndex

  return MaxFlow {..}
  where
    -- be sure to consider reverse edges
    !nEdgesMF = G.length edges * 2

-- TODO: Does `Bounded` work for `Double` for example?

-- | Runs the Dinic's algorithm.
--
-- = Overview
-- Dinic's algorithm loops the following steps:
-- 1. `runMaxFlowBfs`: Creates a DAG with BFS.
-- 2. `runMaxFlowDfs`: Adds flow from the source to the sink.
runMaxFlow ::
  forall c m.
  (U.Unbox c, Num c, Ord c, Bounded c, PrimMonad m) =>
  Vertex ->
  Vertex ->
  MaxFlow (PrimState m) c ->
  m c
runMaxFlow !src !sink container@MaxFlow {..} = do
  bufs@MaxFlowBuffer {..} <-
    -- distsMF, queueMF, iterMF
    MaxFlowBuffer
      <$> UM.unsafeNew nVertsMF
      <*> newBuffer nVertsMF
      <*> U.thaw offsetsMF

  flip fix 0 $ \loopBfs !flow -> do
    -- clear BFS buffers
    GM.set distsMF undefMF
    clearBuffer queueMF
    -- run bfs
    runMaxFlowBfs src sink container bufs

    !distSink <- UM.read distsMF sink
    if distSink == undefMF
      then return flow -- can't increase the flow anymore
      else do
        -- clear dfs buffers
        U.unsafeCopy iterMF offsetsMF
        -- add flow to the `sink` while we can.
        flip fix flow $ \loopDfs f -> do
          !df <- runMaxFlowDfs src sink maxBound container bufs
          if df > 0
            then loopDfs $! f + df
            else loopBfs f

-- | Collect shortest distances from the source vertex using BFS.
runMaxFlowBfs ::
  forall c m.
  (U.Unbox c, Num c, Ord c, PrimMonad m) =>
  Vertex ->
  Vertex ->
  MaxFlow (PrimState m) c ->
  MaxFlowBuffer (PrimState m) c ->
  m ()
runMaxFlowBfs !src !sink MaxFlow {..} MaxFlowBuffer {..} = do
  UM.write distsMF src 0
  pushBack queueMF src
  fix $ \loop ->
    whenJustM (popFront queueMF) $ \v1 -> do
      -- TODO: rather, stop on filling sink?
      !notEnd <- (== undefMF) <$> UM.read distsMF sink
      when notEnd $ do
        let !iStart = offsetsMF G.! v1
            !iEnd = offsetsMF G.! (v1 + 1)

        -- visit neighbors
        !dist1 <- UM.read distsMF v1
        U.forM_ (U.generate (iEnd - iStart) (+ iStart)) $ \i12 -> do
          let !v2 = edgeDstMF G.! i12
          !cap12 <- UM.read edgeCapMF i12
          !notVisited <- (== undefMF) <$> UM.read distsMF v2
          when (cap12 > 0 && notVisited) $ do
            UM.write distsMF v2 (dist1 + 1)
            pushBack queueMF v2

        loop

-- | Modify the flow
runMaxFlowDfs ::
  forall c m.
  (U.Unbox c, Num c, Ord c, PrimMonad m) =>
  Vertex ->
  Vertex ->
  c ->
  MaxFlow (PrimState m) c ->
  MaxFlowBuffer (PrimState m) c ->
  m c
runMaxFlowDfs !v0 !sink !flow0 MaxFlow {..} MaxFlowBuffer {..} = runDfs v0 flow0
  where
    runDfs !v1 !flow
      | v1 == sink = return flow
      | otherwise = fix $ \visitNeighbor -> do
          -- `iterMF` holds neighbor iteration counters
          !i1 <- UM.read iterMF v1
          if i1 >= offsetsMF G.! (v1 + 1)
            then do
              -- visited all the neighbors. no flow
              return 0
            else do
              -- increment the counter to remember the neighbor iteration state:
              UM.write iterMF v1 (i1 + 1)

              -- go if it can flow
              let !v2 = edgeDstMF G.! i1
              !cap12 <- UM.read edgeCapMF i1
              !connected <- (<) <$> UM.read distsMF v1 <*> UM.read distsMF v2
              if cap12 > 0 && connected
                then do
                  -- get to the final flow
                  !flow' <- runDfs v2 $! min flow cap12
                  if flow' > 0
                    then do
                      -- found the sink
                      modifyFlow i1 flow'
                      return flow'
                    else visitNeighbor
                else visitNeighbor

    modifyFlow !i1 !flow = do
      UM.modify edgeCapMF (subtract flow) i1
      UM.modify edgeCapMF (+ flow) (edgeRevIndexMF G.! i1)
