{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | `vector`-based sparse graph implementation. Heavily inspired by @cojna/iota@.
module Data.Graph.Sparse where

import Control.Applicative
import Control.Monad
import Control.Monad.Extra (unlessM, whenM)
import Control.Monad.Fix
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST
import Control.Monad.State.Class
import Control.Monad.Trans.State.Strict (execStateT)
import Data.Buffer
import Data.Graph.Alias (EdgeId, Vertex)
import Data.Graph.Generic
import Data.Maybe
import Data.Ord (comparing)
import Data.Primitive.MutVar
import Data.Tuple.Extra (first3, second3, thd3, third3)
import Data.UnionFind.Mutable
import qualified Data.Vector.Algorithms.Intro as VAI
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Vector.IxVector
import GHC.Stack (HasCallStack)
import ToyLib.Debug (dbgAssert)

-- | CSR (compressed sparse row) representation of a graph, weightened or unweightened.
data SparseGraph w = SparseGraph
  { -- | Number of vertices.
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
buildSG :: Int -> U.Vector (Vertex, Vertex) -> SparseGraph ()
buildSG !nVertsSG = buildWSG nVertsSG . U.map (\(!v1, !v2) -> (v1, v2, ()))

-- | Builds a weightned `SparseGraph`.
{-# INLINE buildWSG #-}
buildWSG :: (UM.Unbox w) => Int -> U.Vector (Vertex, Vertex, w) -> SparseGraph w
buildWSG !nVertsSG !edges =
  let !nEdgesSG = U.length edges
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
adj :: SparseGraph w -> Vertex -> U.Vector Vertex
adj SparseGraph {..} v = U.unsafeSlice o1 (o2 - o1) adjacentsSG
  where
    !o1 = U.unsafeIndex offsetsSG v
    !o2 = U.unsafeIndex offsetsSG (v + 1)

-- | Returns @(EdgeId, Vertex)@ paris. Hardly used.
{-# INLINE eAdj #-}
eAdj :: SparseGraph w -> Vertex -> U.Vector (EdgeId, Vertex)
eAdj SparseGraph {..} v = U.imap ((,) . (+ o1)) vs
  where
    !o1 = U.unsafeIndex offsetsSG v
    !o2 = U.unsafeIndex offsetsSG (v + 1)
    !vs = U.unsafeSlice o1 (o2 - o1) adjacentsSG

-- | Retrieves adjacent vertices with weights.
{-# INLINE adjW #-}
adjW :: (U.Unbox w) => SparseGraph w -> Vertex -> U.Vector (Vertex, w)
adjW SparseGraph {..} v = U.zip vs ws
  where
    !o1 = U.unsafeIndex offsetsSG v
    !o2 = U.unsafeIndex offsetsSG (v + 1)
    !vs = U.unsafeSlice o1 (o2 - o1) adjacentsSG
    !ws = U.unsafeSlice o1 (o2 - o1) edgeWeightsSG

----------------------------------------------------------------------------------------------------
-- DFS / BFS / 01-BFS / Dijkstra
----------------------------------------------------------------------------------------------------

-- | \(O(V+E)\) Depth-first search. Returns a vector of distances to each vertex. Unreachable
-- vertices are given distance of `-1`.
dfsSG :: SparseGraph w -> Vertex -> U.Vector Int
dfsSG gr@SparseGraph {..} !source = U.create $ do
  let !undef = -1 :: Int
  !dist <- UM.replicate nVertsSG undef

  flip fix (0 :: Int, source) $ \loop (!depth, !v1) -> do
    UM.write dist v1 depth
    U.forM_ (gr `adj` v1) $ \v2 -> do
      !d <- UM.read dist v2
      when (d == undef) $ do
        loop (succ depth, v2)

  return dist

-- | \(O(V+E)\) Depth-first search. Just a template.
--
-- = Typical problems
-- - [ABC 317 C - Remembering the Days](https://atcoder.jp/contests/abc317/tasks/abc317_c)
dfsEveryPathSG :: forall w. (Num w, Ord w, U.Unbox w) => SparseGraph w -> Vertex -> w
dfsEveryPathSG gr@SparseGraph {..} !source = runST $ do
  !vis <- UM.replicate nVertsSG False

  flip fix (0 :: w, source) $ \loop (!d1, !v1) -> do
    -- let !_ = dbg (source, v1)
    UM.write vis v1 True
    !v2s <- U.filterM (fmap not . UM.read vis . fst) $ gr `adjW` v1
    !maxDistance <- fmap (U.foldl' max d1) . U.forM v2s $ \(!v2, !w) -> do
      loop (d1 + w, v2)
    UM.write vis v1 False
    return maxDistance

-- | \(O(V+E)\) Collects reachable vertices from source points.
componentsSG :: SparseGraph w -> U.Vector Vertex -> U.Vector Vertex
componentsSG gr sources = runST $ do
  !vis <- UM.replicate (nVertsSG gr) False

  let dfs v1 = do
        unlessM (UM.exchange vis v1 True) $ do
          U.forM_ (gr `adj` v1) $ \v2 -> do
            unlessM (UM.read vis v2) $ do
              dfs v2

  U.forM_ sources dfs
  U.findIndices id <$> U.unsafeFreeze vis

-- | \(O(V+E)\) DFS that paints connnected components. Returns @(vertexToComponentId, components)@.
-- Works on a non-directed graph only.
allComponentsSG :: SparseGraph w -> (U.Vector Int, [[Vertex]])
allComponentsSG gr = runST $ do
  let !n = nVertsSG gr
  components <- UM.replicate n (-1 :: Int)

  iGroupRef <- newMutVar (0 :: Int)

  groupVerts <- (\f -> U.foldM' f [] (U.generate n id)) $ \acc v -> do
    g <- UM.read components v
    if g /= -1
      then return acc
      else do
        iGroup <- readMutVar iGroupRef
        modifyMutVar iGroupRef (+ 1)
        fmap (: acc) . (`execStateT` []) $ flip fix v $ \loop v1 -> do
          UM.write components v1 iGroup
          modify' (v1 :)
          U.forM_ (gr `adj` v1) $ \v2 -> do
            whenM ((== -1) <$> UM.read components v2) $ do
              loop v2

  (,groupVerts) <$> U.unsafeFreeze components

-- | \(O(V+E)\) Breadth-first search. Unreachable vertices are given distance of @-1@.
bfsSG :: SparseGraph w -> Vertex -> U.Vector Int
bfsSG gr@SparseGraph {..} = genericBfs (gr `adj`) nVertsSG

-- | \(O(V+E)\) 01-BFS. Unreachable vertices are given distance of @-1@.
bfs01SG :: SparseGraph Int -> U.Vector Vertex -> U.Vector Int
bfs01SG gr@SparseGraph {..} sources =
  vecIV $ genericBfs01 (0, nVertsSG - 1) (gr `adjW`) nEdgesSG sources

-- | \(O((E+V) \log {V})\) Dijkstra's algorithm.
--
-- >>> let gr = buildWSG 4 (U.fromList [(0, 1, 1 :: Int), (1, 2, 1), (1, 3, 100), (2, 3, 1)])
-- >>> djSG gr (-1 :: Int) (U.singleton 0)
-- [0,1,2,3]
djSG :: forall w. (Num w, Ord w, U.Unbox w) => SparseGraph w -> w -> U.Vector Vertex -> U.Vector w
djSG gr@SparseGraph {..} = genericDj (gr `adjW`) nVertsSG nEdgesSG

----------------------------------------------------------------------------------------------------
-- Path restoration
----------------------------------------------------------------------------------------------------

-- | \(O(V+E)\) Returns a path from the source to the sink in reverse order.
-- Note that it is NOT not the shortest path:
--
-- >>> reverse <$> dfsPathSG (buildSG 4 (U.fromList [(0, 1), (1, 2), (1, 3), (2, 3)])) 0 3
-- Just [0,1,2,3]
dfsPathSG :: SparseGraph w -> Vertex -> Vertex -> Maybe [Vertex]
dfsPathSG gr@SparseGraph {..} !source !sink = runST $ do
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

-- | \(O(V+E)\)Returns a path from the source to the sink in reverse order.
-- Note that it is NOT not the shortest path:
--
-- >>> reverse $ treeDfsPathSG (buildSG 4 (G.fromList [(0, 1), (1, 2), (1, 3), (2, 3)])) 0 3
-- [0,1,2,3]
treeDfsPathSG :: (HasCallStack) => SparseGraph w -> Vertex -> Vertex -> [Vertex]
treeDfsPathSG gr !source !sink = fromJust $ runST $ do
  let !undef = -1 :: Int

  let loop !parent !v1 !stack
        | v1 == sink = do
            return $ Just (v1 : stack)
        | otherwise = do
            flip fix (U.filter (/= parent) $ gr `adj` v1) $ \visitNeighbors v2s -> case G.uncons v2s of
              Nothing -> return Nothing
              Just (!v2, !v2s') -> do
                -- DFS or next neighbor
                (<|>) <$> loop v1 v2 (v1 : stack) <*> visitNeighbors v2s'

  loop undef source []

-- | \(O(V+E)\) depth-first search. Returns a vector of parents. The source vertex or unrechable
-- vertices are given `-1` as their parent.
--
-- >>> createBfsTreeSG (buildSG 4 (U.fromList [(0, 1), (1, 2), (1, 3), (2, 3)])) 0
-- [-1,0,1,1]
--
-- Retrieve a shortest path:
-- >>> let ps = createBfsTreeSG (buildSG 4 (U.fromList [(0, 1), (1, 2), (1, 3), (2, 3)])) 0
-- >>> restorePath ps 3
-- [0,1,3]
createDfsTreeSG :: SparseGraph w -> Vertex -> U.Vector Vertex
createDfsTreeSG gr@SparseGraph {..} !source = U.create $ do
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

-- | \(O(V+E)\) breadth-first search. Returns a vector of parents. The source vertex or unrechable
-- vertices are given `-1` as their parent.
--
-- >>> createBfsTreeSG (buildSG 4 (G.fromList [(0, 1), (1, 2), (1, 3), (2, 3)])) 0
-- [-1,0,1,1]
createBfsTreeSG :: SparseGraph w -> Vertex -> U.Vector Vertex
createBfsTreeSG gr@SparseGraph {..} !source = U.create $ do
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

-- | \(O(V)\) Given a vector of vertex parents, restores path from the source to a sink.
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
-- Digraph
----------------------------------------------------------------------------------------------------

-- | Tries to paint the whole graph (possible not connected) as a digraph.
--
-- @
-- let DigraphInfo isFailure vertColors vertComps compInfos = paintDigraphSG gr
-- @
--
-- = Typical problems
-- - [ABC 282 D - Make Bipartite 2](https://atcoder.jp/contests/abc282/tasks/abc282_d)
-- - [PAST 10 O - 3-順列](https://atcoder.jp/contests/past202203-open/tasks/past202203_o)
data DigraphInfo = DigraphInfo
  { -- | False if any of the connected components is not a digraph.
    isAllDigraphDI :: Bool,
    -- | Vertex -> color (0 or 1)
    vertColorDI :: U.Vector Int,
    -- | Vertex -> component index
    vertComponentDI :: U.Vector Int,
    -- | component -> (n1, n2, isDigraph)
    componentInfoDI :: U.Vector (Int, Int, Bool)
  }

-- | \(O(V+E)\) Tries to paint the whole graph (possible not connected) as a digraph.
-- Works on non-directed graphs only.
digraphSG :: SparseGraph w -> DigraphInfo
digraphSG gr = runST $ do
  let !n = nVertsSG gr
  !allDigraph <- newMutVar True
  !vertColors <- UM.replicate n (-1 :: Int)
  !vertComps <- UM.replicate n (-1 :: Int)
  !compInfo <- UM.replicate n (0 :: Int, 0 :: Int, True)

  !nComps <- (\f -> U.foldM' f (0 :: Int) (U.generate n id)) $ \iComp i -> do
    !isPainted <- (/= -1) <$> UM.read vertColors i
    if isPainted
      then return iComp
      else do
        -- paint
        flip fix (0 :: Int, i) $ \loop (!c1, !v1) -> do
          UM.write vertColors v1 c1
          UM.write vertComps v1 iComp
          if even c1
            then UM.modify compInfo (first3 succ) iComp
            else UM.modify compInfo (second3 succ) iComp

          U.forM_ (gr `adj` v1) $ \v2 -> do
            c2 <- UM.read vertColors v2
            when (c2 == c1) $ do
              -- not a digraph
              writeMutVar allDigraph False
              UM.modify compInfo (third3 (const False)) iComp

            when (c2 == -1) $ do
              loop ((c1 + 1) `mod` 2, v2)

        return $ iComp + 1

  DigraphInfo <$> readMutVar allDigraph <*> U.unsafeFreeze vertColors <*> U.unsafeFreeze vertComps <*> U.unsafeFreeze (UM.take nComps compInfo)

----------------------------------------------------------------------------------------------------
-- Topological sort and strongly connected components
----------------------------------------------------------------------------------------------------

-- | \(O(V+E)\) Topological sort
--
-- Non-referenced vertices come first:
-- >>> let !gr = buildSG 5 $ U.fromList ([(0, 1), (0, 2), (2, 3)] :: [(Int, Int)])
-- >>> topSortSG gr
-- [4,0,2,3,1]
topSortSG :: SparseGraph w -> [Vertex]
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

-- | \(O(V+E)\) Partial running of `topSccSG` over topologically sorted vertices, but for some connected components
-- only.
revTopScc1SG :: forall w m. (PrimMonad m) => SparseGraph w -> UM.MVector (PrimState m) Bool -> Vertex -> m [Vertex]
revTopScc1SG !gr' !vis !v0 = do
  flip fix ([], v0) $ \loop (!acc, !v) -> do
    UM.unsafeRead vis v >>= \case
      True -> return acc
      False -> do
        UM.unsafeWrite vis v True
        !vs <- U.filterM (fmap not . UM.unsafeRead vis) $ gr' `adj` v
        -- Create preorder output:
        (v :) <$> U.foldM' (curry loop) acc vs

-- | \(O(V+E)\) Creates a reverse graph.
revSG :: (U.Unbox w) => SparseGraph w -> SparseGraph w
revSG SparseGraph {..} = buildWSG nVertsSG edges'
  where
    !vws = U.zip adjacentsSG edgeWeightsSG
    -- TODO: Faster?
    !edges' = flip U.concatMap (U.generate nVertsSG id) $ \v1 ->
      let !o1 = U.unsafeIndex offsetsSG v1
          !o2 = U.unsafeIndex offsetsSG (v1 + 1)
          !vw2s = U.unsafeSlice o1 (o2 - o1) vws
       in U.map (\(!v2, !w2) -> (v2, v1, w2)) vw2s

-- | \(O(V+E)\) Collectes strongly connected components, reverse topologically sorted.
-- Upstream vertices come first, e.g., @v1 -> v2 -> v3@.
revTopSccSG :: (U.Unbox w) => SparseGraph w -> [[Int]]
revTopSccSG gr = collectSccPreorderSG $ topSortSG gr
  where
    !gr' = revSG gr

    collectSccPreorderSG :: [Int] -> [[Int]]
    collectSccPreorderSG !topVerts = runST $ do
      !vis <- UM.replicate (nVertsSG gr) False
      filter (not . null) <$> mapM (revTopScc1SG gr' vis) topVerts

-- | \(O(V+E)\) Collectes strongly connected components, topologically sorted.
-- Downstream vertices come first, e.g., @v1 <- v2 <- v3@.
topSccSG :: (U.Unbox w) => SparseGraph w -> [[Int]]
topSccSG = map reverse . revTopSccSG

----------------------------------------------------------------------------------------------------
-- MST (Minimum Spanning Tree)
----------------------------------------------------------------------------------------------------

-- | \(O(E)\) Kruscal's algorithm. Returns edges for building a minimum spanning tree.
--
-- NOTE: User ia assumed to not duplicate the edges.
--
-- If you need to detect if the tree spans all the vertices, be sure to check the length of the edges
-- equals to the number of vertices minus one. Do not try to count the vertices directly, e.g.,
-- @o-o o-o@ contains four connected vertices, but only has two edges. It's apparently not a spanning
-- tree.
--
-- = Typical problems
-- - [Typica 49 - Flip Digits 2](https://atcoder.jp/contests/typical90/tasks/typical90_aw)
{-# INLINE collectMST #-}
collectMST :: (Ord w, U.Unbox w) => Int -> U.Vector (Vertex, Vertex, w) -> U.Vector (Vertex, Vertex, w)
collectMST nVerts edges = runST $ do
  uf <- newMUF nVerts
  flip U.filterM edges' $ \(!v1, !v2, !_) -> do
    unifyMUF uf v1 v2
  where
    edges' = U.modify (VAI.sortBy (comparing thd3)) edges

-- | \(O(E)\) Kruscal's algorithm. Returns a minimum spanning tree.
--
-- NOTE: User ia assumed to not duplicate the edges.
{-# INLINE buildMST #-}
buildMST :: (Ord w, U.Unbox w) => Int -> U.Vector (Vertex, Vertex, w) -> SparseGraph w
buildMST nVerts edges = buildWSG nVerts $ U.concatMap expand $ collectMST nVerts edges
  where
    {-# INLINE expand #-}
    expand (!v1, !v2, !w) = U.fromListN 2 [(v1, v2, w), (v2, v1, w)]
