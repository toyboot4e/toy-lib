{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | `vector`-based sparse graph implementation (weightened or unweightened).
--
-- Heavily inspired by @cojna/iota@.
module Data.Graph.Sparse where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST
import Data.Bifunctor
import Data.Graph (Vertex)
import qualified Data.Heap as H
import qualified Data.IntSet as IS
import Data.Ix
import Data.Unindex
import qualified Data.Vector.Fusion.Stream.Monadic as MS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import ToyLib.Macro (dbgAssert)
import ToyLib.Prelude (foldForM, foldForMVG, foldForVG, rangeMS, rangeVU)

type Edge = (Vertex, Vertex)

-- | Weightened edge
type WEdgeWith w = (Vertex, Vertex, w)

type EdgeId = Int

-- | CSR (compressed sparse row) representation of a graph (weightened or unweightened)
data SparseGraph i w = SparseGraph
  { -- | Vertex index boundary.
    boundsSG :: !(i, i),
    -- | Number of vertices.
    nVertsSG :: !Int,
    -- | Number of edges.
    nEdgesSG :: !Int,
    -- | Maps `Vector` to the starting edge index.
    offsetsSG :: !(VU.Vector Int),
    -- | Adjacent vertices sorted with starting vertex.
    adjacentsSG :: !(VU.Vector Vertex),
    -- | Edge weight information.
    edgeWeightsSG :: !(VU.Vector w)
  }
  deriving (Show)

-- | Builds an unweightned `SparseGraph`.
--
-- TODO: Faster implementation
{-# INLINE buildUSG #-}
buildUSG :: (Unindex i) => (i, i) -> Int -> [(i, i)] -> SparseGraph i ()
buildUSG boundsSG nEdgesSG edges =
  buildRawSG boundsSG nEdgesSG $ map (\(!i1, !i2) -> (ix i1, ix i2, ())) edges
  where
    ix = index boundsSG

-- | Builds a weightned `SparseGraph`.
{-# INLINE buildWSG #-}
buildWSG :: (Unindex i, VUM.Unbox w) => (i, i) -> Int -> [(i, i, w)] -> SparseGraph i w
buildWSG boundsSG nEdgesSG edges =
  buildRawSG boundsSG nEdgesSG $ map (\(!i1, !i2, w) -> (ix i1, ix i2, w)) edges
  where
    ix = index boundsSG

{-# INLINE buildRawSG #-}
buildRawSG :: (Unindex i, VUM.Unbox w) => (i, i) -> Int -> [(Vertex, Vertex, w)] -> SparseGraph i w
buildRawSG boundsSG nEdgesSG edges =
  let !nVertsSG = rangeSize boundsSG
      !offsetsSG = (VU.scanl' (+) 0) $ VU.create $ do
        !outDegs <- VUM.replicate nVertsSG (0 :: Int)
        forM_ edges $ \(!v1, !_, !_) -> do
          VUM.modify outDegs succ v1
        return outDegs

      !_ = dbgAssert (VU.last offsetsSG == nEdgesSG)

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

-- | Retrieves adjacent vertex indices.
{-# INLINE adj #-}
adj :: (Unindex i) => SparseGraph i w -> i -> VU.Vector i
adj gr i = VU.map (unindex (boundsSG gr)) $ adjRaw gr v
  where
    !v = index (boundsSG gr) i

-- | Retrieves adjacent vertices.
{-# INLINE adjRaw #-}
adjRaw :: SparseGraph i w -> Vertex -> VU.Vector Vertex
adjRaw SparseGraph {..} v = VU.unsafeSlice o1 (o2 - o1) adjacentsSG
  where
    !o1 = VU.unsafeIndex offsetsSG v
    !o2 = VU.unsafeIndex offsetsSG (v + 1)

-- | Retrieves adjacent vertices with weights.
{-# INLINE adjW #-}
adjW :: (Unindex i, VU.Unbox w) => SparseGraph i w -> i -> VU.Vector (i, w)
adjW gr i = VU.map (first (unindex (boundsSG gr))) $ adjWRaw gr v
  where
    !v = index (boundsSG gr) i

-- | Retrieves adjacent vertex indices with weights.
{-# INLINE adjWRaw #-}
adjWRaw :: (VU.Unbox w) => SparseGraph i w -> Vertex -> VU.Vector (Vertex, w)
adjWRaw SparseGraph {..} v = VU.zip vs ws
  where
    !o1 = VU.unsafeIndex offsetsSG v
    !o2 = VU.unsafeIndex offsetsSG (v + 1)
    !vs = VU.unsafeSlice o1 (o2 - o1) adjacentsSG
    !ws = VU.unsafeSlice o1 (o2 - o1) edgeWeightsSG

-- TODO: Return IxVector
dfsSG :: (Unindex i) => SparseGraph i w -> i -> VU.Vector Int
dfsSG gr@SparseGraph {..} !startIx = VU.create $ do
  let !undef = -1 :: Int
  !dist <- VUM.replicate nVertsSG undef

  flip fix (0 :: Int, index boundsSG startIx) $ \loop (depth, v1) -> do
    VU.forM_ (gr `adjRaw` v1) $ \v2 -> do
      !d <- VUM.read dist v2
      when (d /= undef) $ do
        VUM.write dist v2 depth
        loop (succ depth, v2)

  return dist

-- TODO: Return IxVector
bfsSG :: (Unindex i) => SparseGraph i w -> i -> VU.Vector Int
bfsSG gr@SparseGraph {..} !startIx = VU.create $ do
  let !undef = -1 :: Int
  !dist <- VUM.replicate nVertsSG undef

  let inner !depth !vs1
        | IS.null vs1 = return ()
        | otherwise = do
            let vs1' = IS.toList vs1
            forM_ vs1' $ \v1 -> do
              VUM.unsafeWrite dist v1 depth

            -- FIXME: Easier iteration?
            !vs2 <- foldForM [] vs1' $ \acc v1 -> do
              foldForMVG acc (gr `adjRaw` v1) $ \acc' v2 -> do
                !d <- VUM.unsafeRead dist v2
                if d == undef
                  then return (v2 : acc')
                  else return acc'

            inner (succ depth) $ IS.fromList vs2

  !_ <- inner (0 :: Int) (IS.singleton (index boundsSG startIx))
  return dist

-- | Dijkstra: $O ( ( E + V ) log ⁡ V ) O((E+V)\log {V})$
djSG :: forall i w. (Unindex i, Num w, Ord w, VU.Unbox w) => SparseGraph i w -> i -> w -> VU.Vector w
djSG !gr@SparseGraph {..} !startIx !undef = VU.create $ do
  !dist <- VUM.replicate nVertsSG undef

  let !heap0 = H.singleton $ H.Entry 0 (index boundsSG startIx)
  flip fix heap0 $ \loop heap -> case H.uncons heap of
    Nothing -> return ()
    Just (entry@(H.Entry cost v), heap') -> do
      (== undef) <$> VUM.read dist v >>= \case
        False -> loop heap'
        True -> do
          VUM.write dist v cost
          !vws <- VU.filterM (fmap (== undef) . VUM.read dist . fst) $ gr `adjWRaw` v
          loop $ VU.foldl' (\h (!v, !w) -> H.insert (merge entry $ H.Entry w v) h) heap' vws

  return dist
  where
    merge :: H.Entry w Vertex -> H.Entry w Vertex -> H.Entry w Vertex
    merge (H.Entry !cost1 !_v1) (H.Entry !cost2 !v2) = H.Entry (cost1 + cost2) v2

topSortSG :: SparseGraph i w -> [Vertex]
topSortSG !gr@SparseGraph {..} = runST $ do
  !vis <- VUM.replicate nVertsSG False

  let dfsM !acc !v = do
        VUM.unsafeRead vis v >>= \case
          True -> return acc
          False -> do
            VUM.unsafeWrite vis v True
            !vs <- VU.filterM (fmap not . VUM.unsafeRead vis) $ gr `adjRaw` v
            -- Create postorder output:
            (v :) <$> VU.foldM dfsM acc vs

  MS.foldM dfsM [] (rangeMS 0 (pred nVertsSG))

-- | Partial running of `scc` over topologically sorted vertices, but for some connected components
-- only.
topScc1SG :: forall i w m. (PrimMonad m) => SparseGraph i w -> VUM.MVector (PrimState m) Bool -> Vertex -> m [Vertex]
topScc1SG !gr' !vis !v0 = do
  flip fix ([], v0) $ \loop (!acc, !v) -> do
    VUM.unsafeRead vis v >>= \case
      False -> return acc
      True -> do
        VUM.unsafeWrite vis v True
        !vs <- VU.filterM (fmap not . VUM.unsafeRead vis) $ gr' `adjRaw` v
        -- Create preorder output:
        (v :) <$> VU.foldM (curry loop) acc vs

-- | Creates a reverse graph.
-- TODO: return weightned graph
revSG :: (Unindex i, VU.Unbox w) => SparseGraph i w -> SparseGraph i w
revSG SparseGraph {..} = buildRawSG boundsSG nEdgesSG edges'
  where
    !vws = VU.zip adjacentsSG edgeWeightsSG
    !edges' = foldForVG [] (rangeVU 0 (pred nVertsSG)) $ \acc v1 ->
      let !o1 = VU.unsafeIndex offsetsSG v1
          !o2 = VU.unsafeIndex offsetsSG (v1 + 1)
          !vw2s = VU.unsafeSlice o1 (o2 - o1) vws
       in VU.foldl' (\acc' (v2, !w2) -> (v2, v1, w2) : acc') acc vw2s

-- | Collectes strongly connected components, topologically sorted.
-- Upstream vertices come first, e.g., @(v1 - v2) -> v3 -> v4@.
topSccSG :: (Unindex i, VU.Unbox w) => SparseGraph i w -> [[Int]]
topSccSG gr = collectSccPreorderSG $ topSortSG gr
  where
    !gr' = revSG gr

    collectSccPreorderSG :: [Int] -> [[Int]]
    collectSccPreorderSG !topVerts = runST $ do
      !vis <- VUM.replicate (nVertsSG gr) False
      filter (not . null) <$> mapM (topScc1SG gr' vis) topVerts