{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | `vector`-based sparse graph implementation. Heavily inspired by @cojna/iota@.
module Data.Graph.Sparse where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST
import Data.Array.IArray
import Data.Bifunctor
import Data.BinaryHeap
import Data.BinaryLifting
import Data.Bool (bool)
import Data.Buffer
import Data.Core.SemigroupAction
import Data.Functor.Identity
import Data.Graph.Alias (EdgeId, Vertex)
import Data.Graph.Tree.Lca (LcaCache, ToParent (..))
import qualified Data.IntMap as IM
import qualified Data.Heap as H
import Data.Maybe
import Control.Monad.State.Class
import Control.Monad.Trans.State.Strict (State, StateT, evalState, evalStateT, execState, execStateT, runState, runStateT)
import Data.Utils.Unindex
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Data.Vector.IxVector
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Stack (HasCallStack)
import ToyLib.Macro (dbgAssert)

-- | CSR (compressed sparse row) representation of a graph, weightened or unweightened.
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

-- | Builds an non-weightned `SparseGraph`.
{-# INLINE buildSG #-}
buildSG :: (Unindex i) => (i, i) -> U.Vector (i, i) -> SparseGraph i ()
buildSG !boundsSG !edges =
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
          UM.modify outDegs (+ 1) v1
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

----------------------------------------------------------------------------------------------------
-- DFS / BFS / 01-BFS / Dijkstra
----------------------------------------------------------------------------------------------------

-- | /O(V+E)/ Depth-first search. Returns a vector of distances to each vertex. Unreachable
-- vertices are given distance of `-1`.
dfsSG :: (Unindex i) => SparseGraph i w -> i -> IxVector i (U.Vector Int)
dfsSG gr@SparseGraph {..} !sourceIx = IxVector boundsSG $ U.create $ do
  let !undef = -1 :: Int
  !dist <- UM.replicate nVertsSG undef

  flip fix (0 :: Int, index boundsSG sourceIx) $ \loop (!depth, !v1) -> do
    UM.write dist v1 depth
    U.forM_ (gr `adj` v1) $ \v2 -> do
      !d <- UM.read dist v2
      when (d == undef) $ do
        loop (succ depth, v2)

  return dist

-- | /O(V+E)/ Depth-first search. Just a template.
--
-- = Typical problems
-- - [ABC 317 C - Remembering the Days](https://atcoder.jp/contests/abc317/tasks/abc317_c)
dfsEveryPathSG :: SparseGraph Int Int -> Int -> Int
dfsEveryPathSG gr@SparseGraph {..} !source = runST $ do
  !vis <- UM.replicate nVertsSG False

  flip fix (0 :: Int, source) $ \loop (!d1, !v1) -> do
    -- let !_ = dbg (source, v1)
    UM.write vis v1 True
    !v2s <- U.filterM (fmap not . UM.read vis . fst) $ gr `adjW` v1
    !maxDistance <- fmap (U.foldl' max (0 :: Int)) . U.forM v2s $ \(!v2, !w) -> do
      loop (d1 + w, v2)
    UM.write vis v1 False
    return $ max d1 maxDistance

-- | A side effect-only not generic-enough DFS.
nonGenericDfs :: (PrimMonad m) => Int -> (Vertex -> m (U.Vector Vertex)) -> (Vertex -> m ()) -> Vertex -> m ()
nonGenericDfs !nVerts !gr !visit !v0 = do
  !vis <- UM.replicate nVerts False
  UM.write vis v0 True
  flip fix v0 $ \loop v1 -> do
    !vs <- gr v1
    U.forM_ vs $ \v2 -> do
      !b <- UM.read vis v2
      unless b $ do
        UM.write vis v2 True
        visit v2
        loop v2
        -- for visiting every path:
        -- UM.write vis v2 False
        return ()

-- | /O(V+E)/ Marks connected vertices. Also consider using union-find tree.
componentsVecSG :: (Ix i) => SparseGraph i w -> i -> IxVector i (U.Vector Bool)
componentsVecSG gr@SparseGraph {..} !sourceIx = IxVector boundsSG $ U.create $ do
  !vis <- UM.replicate nVertsSG False

  flip fix source $ \loop v1 -> do
    UM.write vis v1 True
    let !v2s = gr `adj` v1
    U.forM_ v2s $ \v2 -> do
      !visited <- UM.read vis v2
      unless visited $ do
        loop v2

  return vis
  where
    !source = index boundsSG sourceIx :: Vertex

-- | /O(V+E)/ breadth-first search. Unreachable vertices are given distance of @-1@.
bfsSG :: (Ix i) => SparseGraph i w -> i -> IxVector i (U.Vector Int)
bfsSG gr@SparseGraph {..} !sourceIx =
  IxVector boundsSG $
    genericBfs (gr `adj`) nVertsSG (index boundsSG sourceIx)

-- | /O(V+E)/ breadth-first search. Unreachable vertices are given distance of @-1@.
genericBfs :: (Int -> U.Vector Int) -> Int -> Vertex -> U.Vector Int
genericBfs !gr !nVerts !source = U.create $ do
  let !undef = -1 :: Int
  !dist <- UM.replicate nVerts undef
  !queue <- newBufferAsQueue nVerts

  pushBack queue source
  UM.unsafeWrite dist source (0 :: Int)

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

-- | /O(V+E)/ 01-BFS. Unreachable vertices are given distance of @-1@.
genericBfs01 :: (Ix i, U.Unbox i) => (i, i) -> (i -> U.Vector (i, Int)) -> U.Vector i -> IxUVector i Int
genericBfs01 !bndExt !gr !sources = IxVector bndExt $ U.create $ do
  let !undef = -1 :: Int
  let !nVertsExt = rangeSize bndExt
  !vec <- IxVector bndExt <$> UM.replicate nVertsExt undef
  !deque <- newBufferAsDeque nVertsExt

  U.forM_ sources $ \vExt -> do
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

-- | /O((E+V) \log {V})/ Dijkstra's algorithm.
--
-- >>> let gr = buildWSG (0 :: Int, 3 :: Int) (U.fromList [(0, 1, 1 :: Int), (1, 2, 1), (1, 3, 100), (2, 3, 1)])
-- >>> vecIV $ djSG gr (-1 :: Int) (U.singleton 0)
-- [0,1,2,3]
djSG :: forall i w. (Ix i, U.Unbox i, Num w, Ord w, U.Unbox w) => SparseGraph i w -> w -> U.Vector i -> IxUVector i w
djSG gr@SparseGraph {..} !undef !is0 =
  IxVector boundsSG $
    genericDj (gr `adjW`) nVertsSG nEdgesSG undef (U.map (index boundsSG) is0)

-- | /O((E+V) \log {V})/ Dijkstra's algorithm.
--
-- Does pruning on heap entry pushing: <https://www.slideshare.net/yosupo/ss-46612984> P15
--
-- Note that longest Dijkstra can be implemented by just using a max heap.
genericDj :: forall w. (U.Unbox w, Num w, Ord w) => (Int -> U.Vector (Int, w)) -> Int -> Int -> w -> U.Vector Vertex -> U.Vector w
genericDj !gr !nVerts !nEdges !undef !vs0 = U.create $ do
  !dist <- UM.replicate nVerts undef
  !heap <- newMinBinaryHeap (nEdges + 1)
  -- !last <- UM.replicate nVerts (-1 :: Vertex)

  U.forM_ vs0 $ \v -> do
    UM.write dist v 0
    insertBH heap (0, v)

  fix $ \loop ->
    deleteFindTopBH heap >>= \case
      Nothing -> return ()
      Just (!w1, !v1) -> do
        !newVisit <- (== w1) <$> UM.read dist v1
        when newVisit $ do
          U.forM_ (gr v1) $ \(!v2, !dw2) -> do
            !w2 <- UM.read dist v2
            let !w2' = merge w1 dw2
            when (w2 == undef || w2' < w2) $ do
              UM.write dist v2 w2'
              -- UM.write last v2 v1
              insertBH heap (w2', v2)
        loop

  return dist
  where
    {-# INLINE merge #-}
    merge :: w -> w -> w
    merge = (+)

-- TODO: apply format
-- TODO: test
genericSparseDj :: forall w. (U.Unbox w, Num w, Ord w) => (Int -> U.Vector (Int, w)) -> Int -> Int -> w -> U.Vector Vertex -> IM.IntMap w
genericSparseDj !gr !nVerts !nEdges !undef !vs0 = (`execState` IM.empty) $ do
  U.forM_ vs0 $ \v -> do
    modify' $ IM.insert v 0

  let !heap0 = H.fromList $ V.toList $ V.map (H.Entry 0) $ U.convert vs0
  flip fix heap0 $ \loop !heap ->
    case H.uncons heap of
      Nothing -> return ()
      Just (H.Entry !w1 !v1, !heap') -> do
        !newVisit <- (\case Just w | w == w1 -> True; _ -> False) <$> gets (IM.lookup v1)
        !nextHeap <-
          if newVisit
            then do
              (\f -> U.foldM' f heap' (gr v1)) $ \h (!v2, !dw2) -> do
                !w2 <- fromMaybe undef <$> gets (IM.lookup v2)
                let !w2' = merge w1 dw2
                if (w2 == undef || w2' < w2)
                  then do
                    modify' $ IM.insert v2 w2'
                    return $ H.insert (H.Entry w2' v2) h
                  else do
                    return h
            else do
              return heap'
        loop nextHeap
  where
    {-# INLINE merge #-}
    merge :: w -> w -> w
    merge = (+)

----------------------------------------------------------------------------------------------------
-- Path restoration
----------------------------------------------------------------------------------------------------

-- | Returns a path from the source to the sink in reverse order.
-- Note that it is NOT not the shortest path:
--
-- >>> reverse <$> dfsPathSG (buildSG (0 :: Int, 3 :: Int) (U.fromList [(0, 1), (1, 2), (1, 3), (2, 3)])) 0 3
-- Just [0,1,2,3]
dfsPathSG :: (Unindex i) => SparseGraph i w -> i -> i -> Maybe [Vertex]
dfsPathSG gr@SparseGraph {..} !sourceIx !sinkIx = runST $ do
  let !undef = -1 :: Int
  !dist <- UM.replicate nVertsSG undef

  let loop !depth !v1 !stack = do
        !lastD1 <- UM.read dist v1
        if lastD1 /= undef
          then return Nothing
          else do
            UM.write dist v1 depth
            if v1 == sink
              then return $ Just (v1 : stack)
              else do
                flip fix (gr `adj` v1) $ \visitNeighbors v2s -> case G.uncons v2s of
                  Nothing -> return Nothing
                  Just (!v2, !v2s') -> do
                    -- DFS or next neighbor
                    (<|>) <$> loop (succ depth) v2 (v1 : stack) <*> visitNeighbors v2s'

  loop (0 :: Int) source []
  where
    !source = index boundsSG sourceIx
    !sink = index boundsSG sinkIx

-- | Returns a path from the source to the sink in reverse order.
-- Note that it is NOT not the shortest path:
--
-- >>> reverse $ treeDfsPathSG (buildSG (0 :: Int, 3 :: Int) (G.fromList [(0, 1), (1, 2), (1, 3), (2, 3)])) 0 3
-- [0,1,2,3]
treeDfsPathSG :: (HasCallStack, Unindex i) => SparseGraph i w -> i -> i -> [Vertex]
treeDfsPathSG gr@SparseGraph {..} !sourceIx !sinkIx = fromJust $ runST $ do
  let !undef = -1 :: Int

  let loop !parent !v1 !stack = do
        if v1 == sink
          then return $ Just (v1 : stack)
          else do
            flip fix (U.filter (/= parent) $ gr `adj` v1) $ \visitNeighbors v2s -> case G.uncons v2s of
              Nothing -> return Nothing
              Just (!v2, !v2s') -> do
                -- DFS or next neighbor
                (<|>) <$> loop v1 v2 (v1 : stack) <*> visitNeighbors v2s'

  loop undef source []
  where
    !source = index boundsSG sourceIx
    !sink = index boundsSG sinkIx

-- | /O(V+E)/ depth-first search. Returns a vector of parents. The source vertex or unrechable
-- vertices are given `-1` as their parent.
--
-- >>> createBfsTreeSG (buildSG (0 :: Int, 3 :: Int) (U.fromList [(0, 1), (1, 2), (1, 3), (2, 3)])) 0
-- [-1,0,1,1]
--
-- Retrieve a shortest path:
-- >>> let ps = createBfsTreeSG (buildSG (0 :: Int, 3 :: Int) (U.fromList [(0, 1), (1, 2), (1, 3), (2, 3)])) 0
-- >>> restorePath ps 3
-- [0,1,3]
createDfsTreeSG :: (Unindex i) => SparseGraph i w -> i -> U.Vector Vertex
createDfsTreeSG gr@SparseGraph {..} !sourceIx = U.create $ do
  let !undef = -1 :: Int
  !prev <- UM.replicate nVertsSG undef
  !queue <- newBufferAsQueue nVertsSG

  -- REMARK: We're not creating
  pushBack queue source
  fix $ \loop -> do
    popFront queue >>= \case
      Nothing -> return ()
      Just !v1 -> do
        U.forM_ (gr `adj` v1) $ \v2 -> do
          !p <- UM.unsafeRead prev v2
          when (p == undef) $ do
            UM.unsafeWrite prev v2 v1
            pushBack queue v2
        loop

  return prev
  where
    !source = index boundsSG sourceIx

-- | /O(V+E)/ breadth-first search. Returns a vector of parents. The source vertex or unrechable
-- vertices are given `-1` as their parent.
--
-- >>> createBfsTreeSG (buildSG (0 :: Int, 3 :: Int) (G.fromList [(0, 1), (1, 2), (1, 3), (2, 3)])) 0
-- [-1,0,1,1]
createBfsTreeSG :: (Unindex i) => SparseGraph i w -> i -> U.Vector Vertex
createBfsTreeSG gr@SparseGraph {..} !sourceIx = U.create $ do
  let !undef = -1 :: Int
  !prev <- UM.replicate nVertsSG undef
  !queue <- newBufferAsQueue nVertsSG

  -- REMARK: We're not creating
  pushBack queue source
  fix $ \loop -> do
    popFront queue >>= \case
      Nothing -> return ()
      Just !v1 -> do
        U.forM_ (gr `adj` v1) $ \v2 -> do
          !p <- UM.unsafeRead prev v2
          when (p == undef) $ do
            UM.unsafeWrite prev v2 v1
            pushBack queue v2
        loop

  return prev
  where
    !source = index boundsSG sourceIx

-- | Given a vector of vertex parents, restores path from the source to a sink.
--
-- TODO: restore without reverse?
restorePath :: U.Vector Vertex -> Vertex -> U.Vector Vertex
restorePath !toParent !sink = U.reverse $ U.unfoldr f sink
  where
    f !v
      | v == -2 = Nothing
      | v' == -1 = Just (v, -2)
      | otherwise = Just (v, v')
      where
        v' = toParent U.! v

----------------------------------------------------------------------------------------------------
-- Topological sort and strongly connected components
----------------------------------------------------------------------------------------------------

-- | Topological sort
--
-- Non-referenced vertices come first:
-- >>> let !gr = buildSG ((0, 4) :: (Int, Int)) $ U.fromList ([(0, 1), (0, 2), (2, 3)] :: [(Int, Int)])
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

  U.foldM' dfsM [] (U.generate nVertsSG id)

-- | Partial running of `topSccSG` over topologically sorted vertices, but for some connected components
-- only.
revTopScc1SG :: forall i w m. (PrimMonad m) => SparseGraph i w -> UM.MVector (PrimState m) Bool -> Vertex -> m [Vertex]
revTopScc1SG !gr' !vis !v0 = do
  flip fix ([], v0) $ \loop (!acc, !v) -> do
    UM.unsafeRead vis v >>= \case
      True -> return acc
      False -> do
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
    !edges' = flip U.concatMap (U.generate nVertsSG id) $ \v1 ->
      let !o1 = U.unsafeIndex offsetsSG v1
          !o2 = U.unsafeIndex offsetsSG (v1 + 1)
          !vw2s = U.unsafeSlice o1 (o2 - o1) vws
       in U.map (\(v2, !w2) -> (v2, v1, w2)) vw2s

-- | Collectes strongly connected components, reverse topologically sorted.
-- Upstream vertices come first, e.g., @v1 -> v2 -> v3@.
revTopSccSG :: (Unindex i, U.Unbox w) => SparseGraph i w -> [[Int]]
revTopSccSG gr = collectSccPreorderSG $ topSortSG gr
  where
    !gr' = revSG gr

    collectSccPreorderSG :: [Int] -> [[Int]]
    collectSccPreorderSG !topVerts = runST $ do
      !vis <- UM.replicate (nVertsSG gr) False
      filter (not . null) <$> mapM (revTopScc1SG gr' vis) topVerts

-- | Collectes strongly connected components, topologically sorted.
-- Downstream vertices come first, e.g., @v3 -> v2 -> v1@.
topSccSG :: (Unindex i, U.Unbox w) => SparseGraph i w -> [[Int]]
topSccSG = map reverse . revTopSccSG

----------------------------------------------------------------------------------------------------
-- Tree (API)
----------------------------------------------------------------------------------------------------

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

----------------------------------------------------------------------------------------------------
-- Tree (impl)
----------------------------------------------------------------------------------------------------

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

-- | Folds a tree from one root vertex using postorder DFS.
foldTree :: SparseGraph Int w -> Vertex -> (op -> a -> a) -> (Vertex -> a) -> (a -> op) -> a
foldTree !tree !root !sact_ !acc0At !toOp = runIdentity $ foldTreeImpl tree root sact_ acc0At toOp (\_ _ -> return ())

-- | Folds a tree from one root vertex using postorder DFS, recording all the accumulation values
-- on every vertex.
scanTree :: (G.Vector v a) => SparseGraph Int w -> Vertex -> (op -> a -> a) -> (Vertex -> a) -> (a -> op) -> v a
scanTree !tree !root !sact_ !acc0At !toOp = G.create $ do
  dp <- GM.unsafeNew nVerts
  !_ <- foldTreeImpl tree root sact_ acc0At toOp $ \v a -> do
    GM.unsafeWrite dp v a
  return dp
  where
    !nVerts = rangeSize $! boundsSG tree

-- | Type-restricted `scanTree`.
scanTreeU :: (U.Unbox a) => SparseGraph Int w -> Vertex -> (op -> a -> a) -> (Vertex -> a) -> (a -> op) -> U.Vector a
scanTreeU = scanTree

-- | Type-restricted `scanTree`.
scanTreeV :: SparseGraph Int w -> Vertex -> (op -> a -> a) -> (Vertex -> a) -> (a -> op) -> V.Vector a
scanTreeV = scanTree

-- | \(O(N)\). Folds a tree for every vertex as a root using the rerooting technique.
-- REMARK: `mempty` is used for initial operator value.
--
-- = Typical problems
-- - [Typical 039 - Tree Distance (★5)](https://atcoder.jp/contests/typical90/tasks/typical90_am)
foldTreeAllSG :: forall a op w. (U.Unbox a, U.Unbox op, MonoidAction op a) => SparseGraph Int w -> (Vertex -> a) -> (a -> op) -> U.Vector a
foldTreeAllSG !tree !acc0At !toOp =
  -- Calculate tree DP for one root vertex
  let !treeDp = scanTreeU tree root0 mact acc0At toOp
      !rootDp = U.create $ do
        -- Calculate tree DP for every vertex as a root:
        !dp <- UM.unsafeNew nVerts
        flip fix (-1, op0, root0) $ \runRootDp (!parent, !parentOp, !v1) -> do
          let !children = U.filter (/= parent) $ tree `adj` v1
          let !opL = U.scanl' (\op v2 -> (op <>) . toOp $ treeDp U.! v2) op0 children
          let !opR = U.scanr' (\v2 op -> (<> op) . toOp $ treeDp U.! v2) op0 children

          -- save
          let !x1 = (parentOp <> U.last opL) `mact` acc0At v1
          UM.write dp v1 x1

          flip U.imapM_ children $ \ !i2 !v2 -> do
            let !lrOp = (opL U.! i2) <> (opR U.! succ i2)
            let !v1Acc = (parentOp <> lrOp) `mact` acc0At v2
            runRootDp (v1, toOp v1Acc, v2)

        return dp
   in rootDp
  where
    !nVerts = rangeSize $ boundsSG tree
    !root0 = 0 :: Int
    !op0 = mempty @op

-- | /O(V^3)/ Floyd-Warshall algorith. It uses `max` as relax operator and the second argument is
-- usually like @maxBound `div` 2@.
--
-- It's strict about path connection and invalid paths are ignored.
distsNN :: (U.Unbox w, Num w, Ord w) => Int -> w -> U.Vector (Int, Int, w) -> IxUVector (Int, Int) w
distsNN !nVerts !undef !wEdges = IxVector bnd $ U.create $ do
  !vec <- UM.replicate (nVerts * nVerts) undef

  U.forM_ wEdges $ \(!v1, !v2, !w) -> do
    UM.write vec (index bnd (v1, v2)) w

  -- `forM_ vs repM_
  forM_ [0 .. nVerts - 1] $ \k -> do
    forM_ [0 .. nVerts - 1] $ \i -> do
      forM_ [0 .. nVerts - 1] $ \j -> do
        !x1 <- UM.read vec (index bnd (i, j))
        !x2 <- do
          !tmp1 <- UM.read vec (index bnd (i, k))
          !tmp2 <- UM.read vec (index bnd (k, j))
          return $! bool (tmp1 + tmp2) undef $ tmp1 == undef || tmp2 == undef
        UM.write vec (index bnd (i, j)) $! min x1 x2

  return vec
  where
    bnd :: ((Int, Int), (Int, Int))
    bnd = ((0, 0), (nVerts - 1, nVerts - 1))
