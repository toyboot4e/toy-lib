{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | /O(V^2E)/ max flow algorithm (Dinic's algorithm). Heavily inspired by @cojna/iota@.
--
-- Just run `maxFlowD` to get the result. All the other functions are used internally.
module Data.Dinic where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST
import Data.Buffer
import Data.Graph (Vertex)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- | The state for Dinic's algorithm
--
-- = Internals
-- Internally the graph is stored in a CSR (compressed sparse row).
data Dinic s c = Dinic
  { nVertsD :: !Int,
    nEdgesD :: !Int,
    -- | Source vertex -> initial edge index. Note that edges are sourted by the starting vertex.
    offsetsD :: !(U.Vector Int),
    -- | Edge index -> destination vertex.
    edgeDstD :: !(U.Vector Int),
    -- | Edge index -> reverse edge index.
    edgeRevIndexD :: !(U.Vector Int),
    -- | Edge index -> residual edge capacity.
    edgeCapD :: !(UM.MVector s c)
  }

-- | Mutable states on running Dinic's algorithm.
data DinicBuffer s c = DinicBuffer
  { -- | BFS result.
    distsD :: !(UM.MVector s Int),
    -- | Queue for BFS
    queueD :: !(Buffer s Vertex),
    -- | DFS state for iterating through all the neighbors.
    iterD :: !(UM.MVector s Int)
  }

-- | /O(V^2E)/ max flow algorithm (Dinic's algorithm)
maxFlowD :: (U.Unbox c, Num c, Ord c, Bounded c) => Int -> Int -> Int -> U.Vector (Vertex, Vertex, c) -> c
maxFlowD !nVerts !src !sink !edges = runST $ do
  !dinic <- buildDinic nVerts edges
  runDinic src sink dinic

undefD :: Int
undefD = -1

-- | Builds `Dinic` from edges.
buildDinic ::
  forall c m.
  (U.Unbox c, Num c, PrimMonad m) =>
  Int ->
  U.Vector (Vertex, Vertex, c) ->
  m (Dinic (PrimState m) c)
buildDinic !nVertsD !edges = do
  let !offsetsD = U.scanl' (+) (0 :: Int) $ U.create $ do
        !degs <- UM.replicate nVertsD (0 :: Int)
        G.forM_ edges $ \(!v1, !v2, !_) -> do
          GM.modify degs (+ 1) v1
          GM.modify degs (+ 1) v2
        return degs

  (!edgeDstD, !edgeRevIndexD, !edgeCapD) <- do
    !edgeDst <- UM.replicate nEdgesD undefD
    !edgeRevIndex <- UM.replicate nEdgesD undefD
    !edgeCap <- UM.replicate nEdgesD (0 :: c)

    -- add the edges and the reverse edges
    -- TODO: reuse it as `iterD`
    !edgeCounter <- U.thaw offsetsD
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
    -- GM.write edgeCap i2 0

    (,,edgeCap) <$> G.unsafeFreeze edgeDst <*> G.unsafeFreeze edgeRevIndex

  return Dinic {..}
  where
    -- be sure to consider reverse edges
    !nEdgesD = G.length edges * 2

-- TODO: Does `Bounded` work for `Double` for example?

-- | Runs the Dinic's algorithm.
--
-- = Overview
-- Dinic's algorithm loops the following steps:
-- 1. `runDinicBfs`: Creates a DAG with BFS.
-- 2. `runDinicDfs`: Adds flow from the source to the sink.
runDinic ::
  forall c m.
  (U.Unbox c, Num c, Ord c, Bounded c, PrimMonad m) =>
  Vertex ->
  Vertex ->
  Dinic (PrimState m) c ->
  m c
runDinic !src !sink dinic@Dinic {..} = do
  bufs@DinicBuffer {..} <-
    -- distsD, queueD, iterD
    DinicBuffer
      <$> UM.unsafeNew nVertsD
      <*> newBufferAsQueue nVertsD
      <*> U.thaw offsetsD

  flip fix 0 $ \loopBfs !flow -> do
    -- clear BFS buffers
    GM.set distsD undefD
    clearBuffer queueD
    -- run bfs
    runDinicBfs src sink dinic bufs

    !distSink <- UM.read distsD sink
    if distSink == undefD
      then return flow -- can't increase the flow anymore
      else do
        -- clear dfs buffers
        U.unsafeCopy iterD offsetsD
        -- add flow to the `sink` while we can.
        flip fix flow $ \loopDfs f -> do
          !df <- runDinicDfs src sink maxBound dinic bufs
          if df > 0
            then loopDfs $! f + df
            else loopBfs f

-- | Collect shortest distances from the source vertex using BFS.
runDinicBfs ::
  forall c m.
  (U.Unbox c, Num c, Ord c, PrimMonad m) =>
  Vertex ->
  Vertex ->
  Dinic (PrimState m) c ->
  DinicBuffer (PrimState m) c ->
  m ()
runDinicBfs !src !sink Dinic {..} DinicBuffer {..} = do
  UM.write distsD src 0
  pushBack queueD src
  fix $ \loop ->
    popFront queueD >>= \case
      Nothing -> return ()
      Just !v1 -> do
        -- TODO: rather, stop on filling sink?
        !notEnd <- (== undefD) <$> UM.read distsD sink
        when notEnd $ do
          let !iStart = offsetsD U.! v1
              !iEnd = offsetsD U.! (v1 + 1)

          -- visit neighbors
          !dist1 <- UM.read distsD v1
          U.forM_ (U.generate (iEnd - iStart) (+ iStart)) $ \i12 -> do
            let !v2 = edgeDstD U.! i12
            !cap12 <- UM.read edgeCapD i12
            !notVisited <- (== undefD) <$> UM.read distsD v2
            when (cap12 > 0 && notVisited) $ do
              UM.write distsD v2 (dist1 + 1)
              pushBack queueD v2

          loop

-- | Modify the flow
runDinicDfs ::
  forall c m.
  (U.Unbox c, Num c, Ord c, PrimMonad m) =>
  Vertex ->
  Vertex ->
  c ->
  Dinic (PrimState m) c ->
  DinicBuffer (PrimState m) c ->
  m c
runDinicDfs !v0 !sink !flow0 Dinic {..} DinicBuffer {..} = runDfs v0 flow0
  where
    runDfs !v1 !flow
      | v1 == sink = return flow
      | otherwise = fix $ \visitNeighbor -> do
          -- `iterD` holds neighbor iteration counters
          !i1 <- UM.read iterD v1
          if i1 >= offsetsD U.! (v1 + 1)
            then do
              -- visited all the neighbors. no flow
              return 0
            else do
              -- increment the counter to remember the neighbor iteration state:
              UM.write iterD v1 (i1 + 1)

              -- go if it can flow
              let !v2 = edgeDstD U.! i1
              !cap12 <- UM.read edgeCapD i1
              !connected <- (<) <$> UM.read distsD v1 <*> UM.read distsD v2
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
      UM.modify edgeCapD (subtract flow) i1
      UM.modify edgeCapD (+ flow) (edgeRevIndexD U.! i1)
