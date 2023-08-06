-- | Calculates every shortest path between two vertices (Floyd-Warshall algorithm).
--
-- - NOTE: It's slow. Prefer Dijkstra when possible.
-- - TODO: Faster, general implementation.

module Data.Graph.FloydWarshall where

import Control.Monad
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Graph (Vertex)
import Data.Ix
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import ToyLib.Prelude

-- {{{ Every shortest path (Floyd-Warshall algorithm)

-- Get the shortest path between every pair of the vertices in a weightend graph

-- | Create buffer for the Floyd-Warshapp algorithm
{-# INLINE newFW #-}
newFW :: (PrimMonad m, VU.Unbox cost) => (Vertex -> cost, cost, cost) -> Int -> [(Int, Int)] -> m (VUM.MVector (PrimState m) cost)
newFW (!getCost, !zeroCost, !maxCost) !nVerts !edges = do
  -- REMARK: Boxed array is too slow
  !dp <- VUM.replicate (nVerts * nVerts) maxCost

  -- diagnonal components
  forMS_ (rangeMS 0 (pred nVerts)) $ \ !v ->
    VUM.unsafeWrite dp (ix (v, v)) zeroCost

  -- directly connected vertices
  forM_ edges $ \(!v1, !v2) -> do
    -- let !_ = traceShow (v1, v2, values VU.! v2) ()
    -- (distance, value)
    let !cost = getCost v2
    VUM.unsafeWrite dp (ix (v1, v2)) cost

  return dp
  where
    ix :: (Int, Int) -> Int
    ix = index ((0, 0), (nVerts - 1, nVerts - 1))

{-# INLINE runFW #-}
runFW :: (PrimMonad m, VU.Unbox cost) => (cost -> cost -> cost, cost -> cost -> cost) -> Int -> VUM.MVector (PrimState m) cost -> m ()
runFW (!mergeCost, !minCost) !nVerts !dp = do
  let !ve = pred nVerts
  forM_ (range ((0, 0, 0), (ve, ve, ve))) $ \(!v3, !v1, !v2) -> do
    !cost1 <- VUM.unsafeRead dp (ix (v1, v2))
    !cost2 <- mergeCost <$> VUM.unsafeRead dp (ix (v1, v3)) <*> VUM.unsafeRead dp (ix (v3, v2))
    -- let !_ = traceShow ((v3, v2, v1), cost1, cost2, mergeCost cost1 cost2) ()
    VUM.unsafeWrite dp (ix (v1, v2)) $ minCost cost1 cost2
  where
    ix :: (Int, Int) -> Int
    ix = index ((0, 0), (nVerts - 1, nVerts - 1))

-- Floyd-Warshall algorithm over `WGraph`
-- TODO: test it
-- newFW_W :: (Vertex -> Int) -> Int -> [(Int, Int)] -> IO (VUM.IOVector Int)
-- newFW_W getCost = newFW (getCost, 0 :: Int, maxBound @Int)

-- Floyd-Warshall algorithm over `Graph` + vertex values (see ABC 286 E)
{-# INLINE newFW_ABC286E #-}
newFW_ABC286E :: (PrimMonad m) => (Vertex -> (Int, Int)) -> Int -> [(Int, Int)] -> m (VUM.MVector (PrimState m) (Int, Int))
newFW_ABC286E !getCost = newFW (getCost, (0, 0), (maxBound @Int, maxBound @Int))

{-# INLINE runFW_ABC286E #-}
runFW_ABC286E :: (PrimMonad m) => Int -> VUM.MVector (PrimState m) (Int, Int) -> m ()
runFW_ABC286E = runFW (mergeCost, minCost)
  where
    mergeCost :: (Int, Int) -> (Int, Int) -> (Int, Int)
    mergeCost (!d1, !v1) (!d2, !v2)
      -- if not connected (TODO: use `Maybe` instead of `maxBound`
      | d1 == maxBound = (d1, v1)
      | d2 == maxBound = (d2, v2)
      -- if connected
      | d1 == maxBound = (d1, v1)
      | otherwise = (d1 + d2, v1 + v2)

    minCost :: (Int, Int) -> (Int, Int) -> (Int, Int)
    minCost (!d1, !v1) (!d2, !v2) =
      case compare d1 d2 of
        EQ -> (d1, max v1 v2)
        LT -> (d1, v1)
        GT -> (d2, v2)

-- }}}
