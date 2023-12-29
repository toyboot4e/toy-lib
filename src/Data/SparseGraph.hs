{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | `vector`-based sparse graph implementation (weightened or unweightened).
--
-- Heavily inspired by @cojna/iota@.
module Data.SparseGraph where

import Control.Applicative
import Control.Monad
import Control.Monad.Extra (whenM)
import Control.Monad.Fix
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST
import Data.Array.IArray
import Data.Bifunctor
import Data.BinaryLifting
import Data.Bool (bool)
import Data.Buffer
import Data.Graph (Vertex)
import qualified Data.Heap as H
import Data.Maybe
import Data.Tree.Lca (LcaCache, ToParent (..))
import Data.Tuple.Extra (both)
import Data.Unindex
import qualified Data.Vector.Generic as G
import Data.Vector.IxVector
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Stack (HasCallStack)
import ToyLib.Macro (dbgAssert)
import ToyLib.Prelude (add2, rangeU)

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
    offsetsSG :: !(U.Vector Int),
    -- | Adjacent vertices sorted with starting vertex.
    adjacentsSG :: !(U.Vector Vertex),
    -- | Edge weight information.
    edgeWeightsSG :: !(U.Vector w)
  }
  deriving (Show)

-- | Builds an unweightned `SparseGraph`.
--
-- TODO: Faster implementation
{-# INLINE buildUSG #-}
buildUSG :: (Unindex i) => (i, i) -> U.Vector (i, i) -> SparseGraph i ()
buildUSG !boundsSG !edges =
  buildRawSG boundsSG $ U.map (\(!i1, !i2) -> (ix i1, ix i2, ())) edges
  where
    ix = index boundsSG

-- | Builds a weightned `SparseGraph`.
{-# INLINE buildWSG #-}
buildWSG :: (Unindex i, UM.Unbox w) => (i, i) -> U.Vector (i, i, w) -> SparseGraph i w
buildWSG !boundsSG !edges =
  buildRawSG boundsSG $ U.map (\(!i1, !i2, !w) -> (ix i1, ix i2, w)) edges
  where
    ix = index boundsSG

{-# INLINE buildRawSG #-}
buildRawSG :: (Unindex i, UM.Unbox w) => (i, i) -> U.Vector (Vertex, Vertex, w) -> SparseGraph i w
buildRawSG !boundsSG !edges =
  let !nEdgesSG = U.length edges
      !nVertsSG = rangeSize boundsSG
      !offsetsSG = U.scanl' (+) 0 $ U.create $ do
        !outDegs <- UM.replicate nVertsSG (0 :: Int)
        U.forM_ edges $ \(!v1, !_, !_) -> do
          UM.modify outDegs succ v1
        return outDegs

      !_ = dbgAssert (U.last offsetsSG == nEdgesSG)

      (!adjacentsSG, !edgeWeightsSG) = runST $ do
        !mOffsets <- U.thaw offsetsSG
        !mAdjacents <- UM.unsafeNew nEdgesSG
        !mWeights <- UM.unsafeNew nEdgesSG

        U.forM_ edges $ \(!v1, !v2, !w) -> do
          !iEdgeFlatten <- UM.unsafeRead mOffsets v1
          UM.unsafeWrite mOffsets v1 (iEdgeFlatten + 1)
          UM.unsafeWrite mAdjacents iEdgeFlatten v2
          UM.unsafeWrite mWeights iEdgeFlatten w

        (,) <$> U.unsafeFreeze mAdjacents <*> U.unsafeFreeze mWeights
   in SparseGraph {..}

-- | Retrieves adjacent vertices.
{-# INLINE adj #-}
adj :: SparseGraph i w -> Vertex -> U.Vector Vertex
adj SparseGraph {..} v = U.unsafeSlice o1 (o2 - o1) adjacentsSG
  where
    !o1 = U.unsafeIndex offsetsSG v
    !o2 = U.unsafeIndex offsetsSG (v + 1)

-- | Returns @(EdgeId, Vertex)@ paris. Hardly used.
{-# INLINE eAdj #-}
eAdj :: SparseGraph i w -> Vertex -> U.Vector (EdgeId, Vertex)
eAdj SparseGraph {..} v = U.imap ((,) . (+ o1)) vs
  where
    -- eAdjRaw SparseGraph {..} v = U.imap (\e v -> (e + o1, v)) vs

    !o1 = U.unsafeIndex offsetsSG v
    !o2 = U.unsafeIndex offsetsSG (v + 1)
    !vs = U.unsafeSlice o1 (o2 - o1) adjacentsSG

-- | Retrieves adjacent vertex indices.
{-# INLINE adjIx #-}
adjIx :: (Unindex i) => SparseGraph i w -> i -> U.Vector i
adjIx gr i = U.map (unindex (boundsSG gr)) $ adj gr v
  where
    !v = index (boundsSG gr) i

-- | Retrieves adjacent vertices with weights.
{-# INLINE adjW #-}
adjW :: (U.Unbox w) => SparseGraph i w -> Vertex -> U.Vector (Vertex, w)
adjW SparseGraph {..} v = U.zip vs ws
  where
    !o1 = U.unsafeIndex offsetsSG v
    !o2 = U.unsafeIndex offsetsSG (v + 1)
    !vs = U.unsafeSlice o1 (o2 - o1) adjacentsSG
    !ws = U.unsafeSlice o1 (o2 - o1) edgeWeightsSG

-- | Retrieves adjacent vertex indices with weights.
{-# INLINE adjWIx #-}
adjWIx :: (Unindex i, U.Unbox w) => SparseGraph i w -> i -> U.Vector (i, w)
adjWIx gr i = U.map (first (unindex (boundsSG gr))) $ adjW gr v
  where
    !v = index (boundsSG gr) i

dfsSG :: (Unindex i) => SparseGraph i w -> i -> IxVector i (U.Vector Int)
dfsSG gr@SparseGraph {..} !startIx = IxVector boundsSG $ U.create $ do
  let !undef = -1 :: Int
  !dist <- UM.replicate nVertsSG undef

  flip fix (0 :: Int, index boundsSG startIx) $ \loop (!depth, !v1) -> do
    UM.write dist v1 depth
    U.forM_ (gr `adj` v1) $ \v2 -> do
      !d <- UM.read dist v2
      when (d == undef) $ do
        loop (succ depth, v2)

  return dist

-- | Just a template. Typical problem: [ABC 317 C - Remembering the Days](https://atcoder.jp/contests/abc317/tasks/abc317_c)
dfsEveryPathSG :: SparseGraph Int Int -> Int -> Int
dfsEveryPathSG gr@SparseGraph {..} !start = runST $ do
  !vis <- UM.replicate nVertsSG False

  flip fix (0 :: Int, start) $ \loop (!d1, !v1) -> do
    -- let !_ = dbg (start, v1)
    UM.write vis v1 True
    !v2s <- U.filterM (fmap not . UM.read vis . fst) $ gr `adjW` v1
    !maxDistance <- fmap (U.foldl' max (0 :: Int)) . U.forM v2s $ \(!v2, !w) -> do
      loop (d1 + w, v2)
    UM.write vis v1 False
    return $ max d1 maxDistance

-- | Also consider using union-find tree.
componentsVecSG :: (Ix i) => SparseGraph i w -> i -> IxVector i (U.Vector Bool)
componentsVecSG gr@SparseGraph {..} !startIx = IxVector boundsSG $ U.create $ do
  !vis <- UM.replicate nVertsSG False

  flip fix start $ \loop v1 -> do
    UM.write vis v1 True
    let !v2s = gr `adj` v1
    U.forM_ v2s $ \v2 -> do
      !visited <- UM.read vis v2
      unless visited $ do
        loop v2

  return vis
  where
    !start = index boundsSG startIx :: Vertex

-- | /O(V+E)/ breadth-first search. Unreachable vertices have length of @-1@.
bfsSG :: (Ix i) => SparseGraph i w -> i -> IxVector i (U.Vector Int)
bfsSG gr@SparseGraph {..} !startIx =
  IxVector boundsSG $
    genericBfs (gr `adj`) nVertsSG (index boundsSG startIx)

genericBfs :: (Int -> U.Vector Int) -> Int -> Vertex -> U.Vector Int
genericBfs !gr !nVerts !start = U.create $ do
  let !undef = -1 :: Int
  !dist <- UM.replicate nVerts undef
  !queue <- newBufferAsQueue nVerts

  pushBack queue start
  UM.unsafeWrite dist start (0 :: Int)

  -- procedural programming is great
  fix $ \loop -> do
    popFront queue >>= \case
      Nothing -> return ()
      Just !v1 -> do
        !d1 <- UM.unsafeRead dist v1
        U.forM_ (gr v1) $ \v2 -> do
          !lastD <- UM.unsafeRead dist v2
          when (lastD == undef) $ do
            UM.unsafeWrite dist v2 (d1 + 1)
            pushBack queue v2

        loop

  return dist

-- | TODO: Re-implement with `genericBfs`
bfsGrid317E_MBuffer :: (HasCallStack) => IxUVector (Int, Int) Bool -> (Int, Int) -> IxUVector (Int, Int) Int
bfsGrid317E_MBuffer !isBlock !start = IxVector bounds_ $ runST $ do
  !vis <- IxVector bounds_ <$> UM.replicate (rangeSize bounds_) undef
  !queue <- newBufferAsQueue (rangeSize bounds_)

  pushBack queue start
  writeIV vis start 0

  fix $ \loop ->
    popFront queue >>= \case
      Nothing -> return ()
      Just !yx1 -> do
        !d <- readIV vis yx1
        U.forM_ (nexts yx1) $ \yx2 -> do
          whenM ((== undef) <$> readIV vis yx2) $ do
            writeIV vis yx2 (d + 1)
            pushBack queue yx2
        loop

  U.unsafeFreeze $ vecIV vis
  where
    !undef = -1 :: Int
    !bounds_ = boundsIV isBlock
    nexts !yx0 = U.filter ((&&) <$> inRange bounds_ <*> not . (isBlock @!)) $ U.map (add2 yx0) dyxs
    !dyxs = U.fromList [(1, 0), (-1, 0), (0, 1), (0, -1)]

genericBfs01 :: (Ix i, U.Unbox i) => (i, i) -> (i -> U.Vector (i, Int)) -> U.Vector i -> IxUVector i Int
genericBfs01 !bndExt !gr !starts = IxVector bndExt $ U.create $ do
  let !undef = -1 :: Int
  let !nVertsExt = rangeSize bndExt
  !vec <- IxVector bndExt <$> UM.replicate nVertsExt undef
  !deque <- newBufferAsDeque nVertsExt

  U.forM_ starts $ \vExt -> do
    pushFront deque (0 :: Int, vExt)
    writeIV vec vExt (0 :: Int)

  let step !w0 !vExt0 = do
        !wReserved0 <- readIV vec vExt0
        when (w0 == wReserved0) $ do
          U.forM_ (gr vExt0) $ \(!vExt, !dw) -> do
            let !w = w0 + dw
            !wReserved <- readIV vec vExt
            when (wReserved == undef || w < wReserved) $ do
              writeIV vec vExt w
              if dw == 0
                then pushFront deque (w, vExt)
                else pushBack deque (w, vExt)

  -- generic BFS = pop loop
  fix $ \loop ->
    popFront deque >>= \case
      Nothing -> return ()
      Just (!w, !vExt) -> do
        step w vExt
        loop

  return $ vecIV vec

-- 01-BFS: zero cost with same direction.
bfs01_grid4_typical043 :: (HasCallStack) => IxUVector (Int, Int) Bool -> (Int, Int) -> IxUVector (Int, Int, Int) Int
bfs01_grid4_typical043 !isBlock !start = IxVector boundsExt $ U.create $ do
  -- vec @! (dir, y, x)
  !vec <- IxVector boundsExt <$> UM.replicate (4 * nVerts) undef

  let !redundantSpace = 0
  !deque <- newBufferAsDeque (redundantSpace + 4 * nVerts)
  forM_ [0 .. 3] $ \iDir -> do
    let !vExt = (iDir, fst start, snd start)
    pushFront deque (0 :: Int, vExt)
    writeIV vec vExt (0 :: Int)

  let extract !w0 vExt0@(!iDir0, !y0, !x0) = do
        !wReserved0 <- readIV vec vExt0
        when (w0 == wReserved0) $ do
          U.iforM_ dirs $ \iDir (!dy, !dx) -> do
            let !v = (y0 + dy, x0 + dx)
            when (inRange bounds_ v && not (isBlock @! v)) $ do
              let !w = bool (w0 + 1) w0 (iDir == iDir0)
              let !vExt = (iDir, y0 + dy, x0 + dx)
              !wReserved <- readIV vec vExt
              when (wReserved == undef || w < wReserved) $ do
                writeIV vec vExt w
                if iDir == iDir0
                  then pushFront deque (w, vExt)
                  else pushBack deque (w, vExt)

  -- generic BFS = pop loop
  fix $ \loop ->
    popFront deque >>= \case
      Nothing -> return ()
      Just (!w, !v) -> do
        extract w v
        loop

  return $ vecIV vec
  where
    !undef = -1 :: Int
    (!height, !width) = both succ . snd $ boundsIV isBlock
    !bounds_ = boundsIV isBlock
    !boundsExt = ((0, 0, 0), (3, height - 1, width - 1))
    !nVerts = rangeSize bounds_
    !dirs = U.fromList [(0, 1), (0, -1), (1, 0), (-1, 0)]

-- | Dijkstra: $O((E+V) \log {V})$
--
-- Do pruning on heap entry pushing: <https://www.slideshare.net/yosupo/ss-46612984> P15
djSG :: forall i w. (Ix i, U.Unbox i, Num w, Ord w, U.Unbox w) => SparseGraph i w -> w -> U.Vector i -> IxUVector i w
djSG gr@SparseGraph {..} !undef !is0 =
  IxVector boundsSG $
    genericDj (gr `adjW`) nVertsSG undef (U.map (index boundsSG) is0)

-- TODO: compare the speed of `djSG` via the generic verison
genericDj :: forall w. (U.Unbox w, Num w, Ord w) => (Int -> U.Vector (Int, w)) -> Int -> w -> U.Vector Int -> U.Vector w
genericDj !gr !nVerts !undef !vs0 = U.create $ do
  !dist <- UM.replicate nVerts undef

  let !heap0 = H.fromList $ map (H.Entry 0) (U.toList vs0) :: H.Heap (H.Entry w Int)
  U.forM_ vs0 $ \v -> do
    UM.write dist v 0

  flip fix heap0 $ \loop heap -> case H.uncons heap of
    Nothing -> return ()
    Just (H.Entry !w1 !v1, heap') -> do
      (w1 >) <$> UM.read dist v1 >>= \case
        -- more efficient path was already visited
        True -> loop heap'
        False -> do
          loop <=< (\f -> U.foldM' f heap' (gr v1)) $ \h (!v2, !dw2) -> do
            !w2 <- UM.read dist v2
            let !w2' = merge w1 dw2
            if w2 == undef || w2' < w2
              then do
                UM.write dist v2 w2'
                return $ H.insert (H.Entry w2' v2) h
              else return h

  return dist
  where
    merge :: w -> w -> w
    merge = (+)

-- TODO: BFS with path restoration

-- | Returns a path from source to the sink in reverse order.
-- Note that it is NOT not the shortest path:
--
-- >>> reverse <$> dfsPathSG (buildUSG (0 :: Int, 3 :: Int) (G.fromList [(0, 1), (1, 2), (1, 3), (2, 3)])) 0 3
-- Just [0,1,2,3]
dfsPathSG :: (Unindex i) => SparseGraph i w -> i -> i -> Maybe [Vertex]
dfsPathSG gr@SparseGraph {..} !sourceIx !sinkIx = runST $ do
  let !undef = -1 :: Int
  !dist <- UM.replicate nVertsSG undef

  flip fix (0 :: Int, source, []) $ \loop (!depth, !v1, !stack) -> do
    !lastD1 <- UM.read dist v1

    if lastD1 /= undef
      then return Nothing
      else do
        UM.write dist v1 depth
        if v1 == sink
          then return $ Just (v1 : stack)
          else do
            -- visit neighbors one by one, but stop soon on end.
            flip fix (gr `adj` v1) $ \visitNeighbors v2s -> case G.uncons v2s of
              Nothing -> return Nothing
              Just (!v2, !v2s') -> do
                -- DFS or next neighbor
                (<|>) <$> loop (succ depth, v2, v1 : stack) <*> visitNeighbors v2s'
  where
    !source = index boundsSG sourceIx
    !sink = index boundsSG sinkIx

-- | Returns a list of a route in reverse order (a route from sink to source).
-- Note that it is NOT not the shortest path:
--
-- >>> reverse $ treeDfsPathSG (buildUSG (0 :: Int, 3 :: Int) (G.fromList [(0, 1), (1, 2), (1, 3), (2, 3)])) 0 3
-- [0,1,2,3]
treeDfsPathSG :: (HasCallStack, Unindex i) => SparseGraph i w -> i -> i -> [Vertex]
treeDfsPathSG gr@SparseGraph {..} !sourceIx !sinkIx = fromJust $ runST $ do
  let !undef = -1 :: Int

  flip fix (undef, source, []) $ \loop (!parent, !v1, !stack) -> do
    if v1 == sink
      then return $ Just (v1 : stack)
      else do
        flip fix (U.filter (/= parent) $ gr `adj` v1) $ \visitNeighbors v2s -> case G.uncons v2s of
          Nothing -> return Nothing
          Just (!v2, !v2s') -> do
            -- DFS or next neighbor
            (<|>) <$> loop (v1, v2, v1 : stack) <*> visitNeighbors v2s'
  where
    !source = index boundsSG sourceIx
    !sink = index boundsSG sinkIx

-- | Topological sort
--
-- Non-referenced vertices come first:
-- >>> let !gr = buildUSG ((0, 4) :: (Int, Int)) $ U.fromList ([(0, 1), (0, 2), (2, 3)] :: [(Int, Int)])
-- >>> topSortSG gr
-- [4,0,2,3,1]
topSortSG :: SparseGraph i w -> [Vertex]
topSortSG gr@SparseGraph {..} = runST $ do
  !vis <- UM.replicate nVertsSG False

  let dfsM !acc !v = do
        UM.unsafeRead vis v >>= \case
          True -> return acc
          False -> do
            UM.unsafeWrite vis v True
            !vs <- U.filterM (fmap not . UM.unsafeRead vis) $ gr `adj` v
            -- Create postorder output:
            (v :) <$> U.foldM' dfsM acc vs

  U.foldM' dfsM [] (rangeU 0 (pred nVertsSG))

-- | Partial running of `scc` over topologically sorted vertices, but for some connected components
-- only.
topScc1SG :: forall i w m. (PrimMonad m) => SparseGraph i w -> UM.MVector (PrimState m) Bool -> Vertex -> m [Vertex]
topScc1SG !gr' !vis !v0 = do
  flip fix ([], v0) $ \loop (!acc, !v) -> do
    UM.unsafeRead vis v >>= \case
      False -> return acc
      True -> do
        UM.unsafeWrite vis v True
        !vs <- U.filterM (fmap not . UM.unsafeRead vis) $ gr' `adj` v
        -- Create preorder output:
        (v :) <$> U.foldM' (curry loop) acc vs

-- | Creates a reverse graph.
-- TODO: return weightned graph
revSG :: (Unindex i, U.Unbox w) => SparseGraph i w -> SparseGraph i w
revSG SparseGraph {..} = buildRawSG boundsSG edges'
  where
    !vws = U.zip adjacentsSG edgeWeightsSG
    -- TODO: Faster?
    !edges' = flip U.concatMap (rangeU 0 (pred nVertsSG)) $ \v1 ->
      let !o1 = U.unsafeIndex offsetsSG v1
          !o2 = U.unsafeIndex offsetsSG (v1 + 1)
          !vw2s = U.unsafeSlice o1 (o2 - o1) vws
       in U.map (\(v2, !w2) -> (v2, v1, w2)) vw2s

-- | Collectes strongly connected components, topologically sorted.
-- Upstream vertices come first, e.g., @(v1 - v2) -> v3 -> v4@.
topSccSG :: (Unindex i, U.Unbox w) => SparseGraph i w -> [[Int]]
topSccSG gr = collectSccPreorderSG $ topSortSG gr
  where
    !gr' = revSG gr

    collectSccPreorderSG :: [Int] -> [[Int]]
    collectSccPreorderSG !topVerts = runST $ do
      !vis <- UM.replicate (nVertsSG gr) False
      filter (not . null) <$> mapM (topScc1SG gr' vis) topVerts

-- | LCA component. See also `lca` and `lcaLen` from `Data.Tree`.
treeDepthInfoSG :: SparseGraph Int w -> Int -> (ToParent, U.Vector Int)
treeDepthInfoSG gr@SparseGraph {..} !root = runST $ do
  !parents <- UM.replicate nVerts (-1 :: Int)
  !depths <- UM.replicate nVerts (-1 :: Int)

  flip fix (0 :: Int, -1 :: Int, U.singleton root) $ \loop (!depth, !parent, !vs) -> do
    U.forM_ vs $ \v -> do
      UM.unsafeWrite depths v depth
      UM.unsafeWrite parents v parent
      let !vs' = U.filter (/= parent) $ gr `adj` v
      loop (succ depth, v, vs')

  (,) <$> (ToParent <$> U.unsafeFreeze parents) <*> U.unsafeFreeze depths
  where
    !nVerts = rangeSize boundsSG

-- | LCA component. Returns `LcaCache`, i.e., `(parents, depths, parents')`.
lcaCacheSG :: SparseGraph Int w -> Vertex -> LcaCache
lcaCacheSG !gr !root = (toParent, depths, toParentN)
  where
    (!toParent, !depths) = treeDepthInfoSG gr root
    !toParentN = newBinLift toParent
