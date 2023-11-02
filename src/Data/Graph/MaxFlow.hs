{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- | Maximum flow calculation (Ford-Fulkerson algorithm).
module Data.Graph.MaxFlow where

import Control.Monad
import Data.Bifunctor
import Data.Graph (Vertex)
import qualified Data.IntMap.Strict as IM
import Data.Tuple.Extra hiding (first, second)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed.Base as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- {{{ Maximum flow (Ford-Fulkerson algorithm)

-- Find the maximum flow from one vertex to another by repeatedly finding augument path and teaking
-- the flow.

-- TODO: Use `ST` monad for the visit buffer.. but how? (ST monad transformer??)

-- | Edge in residual network from on vertex to another.
data RNEdge = RNEdge
  { -- | Points the the other side of the edge
    to :: {-# UNPACK #-} !Vertex,
    -- | Capacity of the edge, or the flow from the vertex to another
    cap :: {-# UNPACK #-} !Int,
    -- | The other side of the vertices is pointed with `rn ! (rev (rn ! to))`
    -- so that edge insertion takes just $O(1)$.
    rev :: {-# UNPACK #-} !Int
  }
  deriving (Show)

instance U.IsoUnbox RNEdge (Int, Int, Int) where
  {-# INLINE toURepr #-}
  toURepr (RNEdge !x1 !x2 !x3) = (x1, x2, x3)
  {-# INLINE fromURepr #-}
  fromURepr (!x1, !x2, !x3) = RNEdge x1 x2 x3

newtype instance U.MVector s RNEdge = MV_RNEdge (UM.MVector s (Int, Int, Int))

newtype instance U.Vector RNEdge = V_RNEdge (U.Vector (Int, Int, Int))

deriving via (RNEdge `U.As` (Int, Int, Int)) instance GM.MVector UM.MVector RNEdge

deriving via (RNEdge `U.As` (Int, Int, Int)) instance G.Vector U.Vector RNEdge

instance U.Unbox RNEdge

-- | @Vertex -> [RNEdge]@.
--
-- TODO: For the sub containers, use `Sequence` or something better
type ResidualNetwork = VM.IOVector (IM.IntMap RNEdge)

-- | Builds a residual network at initial state from given edges `(v2, cost)`.
-- {-# INLINE buildRN #-}
-- TODO: make it generic over ST.. for no reason?
buildRN :: Int -> [(Int, (Int, Int))] -> IO ResidualNetwork
buildRN !nVerts !edges = do
  !rn <- VM.replicate nVerts IM.empty
  -- TODO: consider using `U.accumlate` instead?
  forM_ edges $ \(!v1, (!v2, !cap_)) -> do
    addEdgeRN rn v1 v2 cap_
  return rn
  where
    addEdgeRN :: ResidualNetwork -> Int -> Int -> Int -> IO ()
    addEdgeRN !rn !v1 !v2 !maxFlow = do
      !edges1 <- VM.read rn v1
      !edges2 <- VM.read rn v2

      -- REMARK: Be sure to use `insertWith`!
      -- We can have both (v1 -> v2) path and (v2 -> v1) path

      -- We can run up to `maxFlow`:
      VM.write rn v1 $ IM.insertWith mergeEdge v2 (RNEdge v2 maxFlow v1) edges1
      -- We cannot reverse when there's no flow:
      VM.write rn v2 $ IM.insertWith mergeEdge v1 (RNEdge v1 0 v2) edges2

    mergeEdge :: RNEdge -> RNEdge -> RNEdge
    mergeEdge (RNEdge !to_ !flow !cap_) (RNEdge !_ !flow' !_) = RNEdge to_ (flow + flow') cap_

-- | Calculates max flow.
{-# INLINE maxFlowRN #-}
maxFlowRN :: Int -> ResidualNetwork -> Int -> Int -> IO Int
maxFlowRN !nVerts !rn !v0 !ve = do
  !vis <- VM.replicate nVerts False
  inner vis
  where
    inner :: VM.IOVector Bool -> IO Int
    inner !vis =
      augumentPath rn vis v0 ve >>= \case
        Nothing -> return 0
        Just (!flow, !path) -> do
          updateFlow rn flow path
          VM.set vis False
          (flow +) <$> inner vis

-- | Find a flow augment path between two vertices.
{-# INLINE augumentPath #-}
augumentPath :: ResidualNetwork -> VM.IOVector Bool -> Vertex -> Int -> IO (Maybe (Int, [(Vertex, Vertex)]))
augumentPath !rn !vis !v0 !goal = visitVertex v0 (maxBound @Int)
  where
    visitVertex :: Vertex -> Int -> IO (Maybe (Int, [(Vertex, Vertex)]))
    visitVertex !v !flow
      | v == goal = return $ Just (flow, [])
      | otherwise = do
          VM.write vis v True
          !edges <- VM.read rn v
          foldM (step v flow) Nothing edges

    step :: Vertex -> Int -> Maybe (Int, [(Vertex, Vertex)]) -> RNEdge -> IO (Maybe (Int, [(Vertex, Vertex)]))
    step !_ !_ r@(Just _) _ = return r
    step !from !flow !_ !edge = do
      !visited <- VM.read vis (to edge)
      if visited || flow' == 0
        then return Nothing
        else
          visitVertex (to edge) flow' >>= \case
            Nothing -> return Nothing
            Just (!f, !path) -> return $ Just (f, p : path)
      where
        flow' = min flow (cap edge)
        p = (from, to edge)

{-# INLINE updateFlow #-}
updateFlow :: ResidualNetwork -> Int -> [(Vertex, Vertex)] -> IO ()
updateFlow !rn !flow !path = forM_ path $ \(!v1, !v2) -> addFlowRNEdge rn v1 v2 flow

{-# INLINE addFlowRNEdge #-}
addFlowRNEdge :: ResidualNetwork -> Vertex -> Vertex -> Int -> IO ()
addFlowRNEdge !rn !v1 !v2 !flow = do
  -- TODO: consider using `VM.modify`
  -- TODO: consider using `lens`, `snd2` (or not)
  -- TODO: replace `dupe` with function applicative?
  (!edges1, !edge12) <- second (IM.! v2) . dupe <$> VM.read rn v1
  (!edges2, !edge21) <- second (IM.! v1) . dupe <$> VM.read rn v2
  -- let !_ = traceShow ("edge", "v1:", v1, edge12, "v2:", v2, edge21, flow) ()

  -- TODO: debugAssert
  -- when (cap edge12 < flow) $ error "invariant broken"
  VM.write rn v1 $ IM.insert v2 (RNEdge (to edge12) (cap edge12 - flow) (rev edge12)) edges1
  VM.write rn v2 $ IM.insert v1 (RNEdge (to edge21) (cap edge21 + flow) (rev edge21)) edges2

-- }}}
