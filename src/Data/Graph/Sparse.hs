{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | `vector`-based sparse graph implementation. Heavily inspired by @cojna/iota@.
--
-- - TODO: move tree functions to the generic module
module Data.Graph.Sparse where

import Control.Applicative
import Control.Monad
import Control.Monad.Cont (callCC, evalContT)
import Control.Monad.Extra (unlessM)
import Control.Monad.Fix
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST
import Data.Buffer
import Data.Graph.Alias (EdgeId, Vertex)
import Data.Graph.Generic
import Data.Maybe
import Data.Ord (comparing)
import Data.Tuple.Extra (first3, second3, thd3, third3)
import Data.UnionFind.Mutable
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VAI
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Data.Vector.IxVector
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import ToyLib.Debug (dbgAssert)

-- | CSR (compressed sparse row) representation of a graph, weightened or unweightened.
data SparseGraph w = SparseGraph
  { -- | Number of vertices.
    nVertsSG :: {-# UNPACK #-} !Int,
    -- | Number of edges.
    nEdgesSG :: {-# UNPACK #-} !Int,
    -- | Maps `Vector` to the starting edge index.
    offsetsSG :: !(U.Vector Int),
    -- | Adjacent vertices sorted with starting vertex.
    adjacentsSG :: !(U.Vector Vertex),
    -- | Edge weight information.
    edgeWeightsSG :: !(U.Vector w)
  }
  deriving (Show)

-- | \(O(N)\) Builds an non-weightned `SparseGraph`.
{-# INLINE buildSG #-}
buildSG :: Int -> U.Vector (Vertex, Vertex) -> SparseGraph Int
buildSG !nVertsSG = buildWSG nVertsSG . U.map (\(!v1, !v2) -> (v1, v2, 1))

-- | \(O(N)\) Builds an non-weightned `SparseGraph`.
{-# INLINE buildSG_ #-}
buildSG_ :: Int -> U.Vector (Vertex, Vertex) -> SparseGraph ()
buildSG_ !nVertsSG = buildWSG nVertsSG . U.map (\(!v1, !v2) -> (v1, v2, ()))

-- | \(O(N)\) Builds a weightned `SparseGraph`.
{-# INLINE buildWSG #-}
buildWSG :: (UM.Unbox w) => Int -> U.Vector (Vertex, Vertex, w) -> SparseGraph w
buildWSG !nVertsSG !edges =
  let !nEdgesSG = U.length edges
      !offsetsSG = U.scanl' (+) 0 $ U.create $ do
        !outDegs <- UM.replicate nVertsSG (0 :: Int)
        U.forM_ edges $ \(!v1, !_, !_) -> do
          UM.modify outDegs (+ 1) v1
        pure outDegs

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

-- | \(O(1)\) Retrieves adjacent vertices.
{-# INLINE adj #-}
adj :: SparseGraph w -> Vertex -> U.Vector Vertex
adj SparseGraph {..} v = U.unsafeSlice o1 (o2 - o1) adjacentsSG
  where
    !o1 = U.unsafeIndex offsetsSG v
    !o2 = U.unsafeIndex offsetsSG (v + 1)

-- | \(O(1)\) Returns @(EdgeId, Vertex)@ pairs. Hardly used.
{-# INLINE eAdj #-}
eAdj :: SparseGraph w -> Vertex -> U.Vector (EdgeId, Vertex)
eAdj SparseGraph {..} v = U.imap ((,) . (+ o1)) vs
  where
    !o1 = U.unsafeIndex offsetsSG v
    !o2 = U.unsafeIndex offsetsSG (v + 1)
    !vs = U.unsafeSlice o1 (o2 - o1) adjacentsSG

-- | \(O(1)\) Retrieves adjacent vertices with weights.
{-# INLINE adjW #-}
adjW :: (U.Unbox w) => SparseGraph w -> Vertex -> U.Vector (Vertex, w)
adjW SparseGraph {..} v = U.zip vs ws
  where
    !o1 = U.unsafeIndex offsetsSG v
    !o2 = U.unsafeIndex offsetsSG (v + 1)
    !vs = U.unsafeSlice o1 (o2 - o1) adjacentsSG
    !ws = U.unsafeSlice o1 (o2 - o1) edgeWeightsSG

-- * Graph search (DFS, BFS, 01-BFS, Dijkstra)

-- | \(O(V+E)\) Collects reachable vertices from source points.
componentsSG :: SparseGraph w -> U.Vector Vertex -> U.Vector Vertex
componentsSG gr sources = genericComponentsOf (gr `adj`) (nVertsSG gr) sources

-- | \(O(V+E)\) DFS that paints connnected components. Returns @(vertexToComponentId, components)@.
-- Works on a non-directed graph only.
groupSG :: SparseGraph w -> (U.Vector Int, [[Vertex]])
groupSG gr = genericGrouping (gr `adj`) (nVertsSG gr)

-- | \(O(V+E)\) Depth-first search. Returns a vector of distances to each vertex. Unreachable
-- vertices are given distance of `-1`.
dfsSG :: (U.Unbox w, Num w, Eq w) => SparseGraph w -> Vertex -> w -> U.Vector w
dfsSG gr@SparseGraph {..} !source !undefW = genericDfs (gr `adjW`) nVertsSG source undefW

-- | \(O(N N!)\) Depth-first search that finds the longest path. Just a template!
--
-- = Typical problems
-- - [ABC 317 C - Remembering the Days](https://atcoder.jp/contests/abc317/tasks/abc317_c)
dfsEveryPathLongestSG :: forall w. (Num w, Ord w, U.Unbox w) => SparseGraph w -> Vertex -> w
dfsEveryPathLongestSG gr !source = genericDfsLongestPath (gr `adjW`) (nVertsSG gr) source

-- | \(O(V+E)\) Breadth-first search. Unreachable vertices are given distance of @-1@.
bfsSG :: (U.Unbox w, Num w, Eq w) => SparseGraph w -> Vertex -> w -> U.Vector w
bfsSG gr@SparseGraph {..} undefW = genericBfs (gr `adjW`) nVertsSG undefW

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

-- * Path restoration

-- | \(O(V+E)\) depth-first search. Returns a vector of parents. The source vertex or unrechable
-- vertices are given `-1` as their parent. Note that it doesn't return the shortest path.
dfsTreeSG :: (U.Unbox w, Num w) => SparseGraph w -> Vertex -> w -> (U.Vector w, U.Vector Vertex)
dfsTreeSG !gr !source !undefW = genericDfsTree (gr `adjW`) (nVertsSG gr) source undefW

-- | \(O(V+E)\) breadth-first search. Returns a vector of distances and parents. The source vertex
-- or unrechable vertices are given `-1` as their parent.
--
-- >>> bfsTreeSG (buildSG 4 (U.fromList [(0, 1), (1, 2), (1, 3), (2, 3)])) 0 (-1)
-- ([0,1,2,2],[-1,0,1,1])
--
-- Retrieve a shortest path:
-- >>> let (_, ps) = bfsTreeSG (buildSG 4 (U.fromList [(0, 1), (1, 2), (1, 3), (2, 3)])) 0 (-1)
-- >>> restorePath ps 3
-- [0,1,3]
bfsTreeSG :: (U.Unbox w, Num w, Eq w) => SparseGraph w -> Vertex -> w -> (U.Vector w, U.Vector Vertex)
bfsTreeSG gr !source !undefW = genericBfsTree (gr `adjW`) (nVertsSG gr) source undefW

-- | \(O((E+V) \log {V})\) Dijkstra's algorithm with path restoration information.
djTreeSG :: forall w. (Num w, Ord w, U.Unbox w) => SparseGraph w -> w -> U.Vector Vertex -> (U.Vector w, U.Vector Vertex)
djTreeSG gr@SparseGraph {..} = genericDjTree (gr `adjW`) nVertsSG nEdgesSG

-- * Digraph

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
    isAllDigraphDI :: !Bool,
    -- | Vertex -> color (0 or 1)
    vertColorDI :: !(U.Vector Int),
    -- | Vertex -> component index
    vertComponentDI :: !(U.Vector Int),
    -- | component -> (n1, n2, isDigraph)
    componentInfoDI :: !(U.Vector (Int, Int, Bool))
  }

-- | \(O(V+E)\) Tries to paint the whole graph (possible not connected) as a digraph.
-- Works on non-directed graphs only.
digraphSG :: SparseGraph w -> DigraphInfo
digraphSG gr = runST $ do
  let !n = nVertsSG gr
  !allDigraph <- UM.replicate 1 True
  !vertColors <- UM.replicate n (-1 :: Int)
  !vertComps <- UM.replicate n (-1 :: Int)
  !compInfo <- UM.replicate n (0 :: Int, 0 :: Int, True)

  !nComps <- (\f -> U.foldM' f (0 :: Int) (U.generate n id)) $ \iComp i -> do
    !isPainted <- (/= -1) <$> UM.read vertColors i
    if isPainted
      then pure iComp
      else do
        -- paint
        flip fix (0 :: Int, i) $ \loop (!c1, !v1) -> do
          GM.write vertColors v1 c1
          GM.write vertComps v1 iComp
          if even c1
            then UM.modify compInfo (first3 succ) iComp
            else UM.modify compInfo (second3 succ) iComp

          U.forM_ (gr `adj` v1) $ \v2 -> do
            c2 <- UM.read vertColors v2
            when (c2 == c1) $ do
              -- not a digraph
              UM.unsafeWrite allDigraph 0 False
              UM.modify compInfo (third3 (const False)) iComp

            when (c2 == -1) $ do
              loop ((c1 + 1) `mod` 2, v2)

        pure $ iComp + 1

  DigraphInfo <$> UM.unsafeRead allDigraph 0 <*> U.unsafeFreeze vertColors <*> U.unsafeFreeze vertComps <*> U.unsafeFreeze (UM.take nComps compInfo)

-- * Topological sort and strongly connected components

-- | \(O(V+E)\) Topological sort
--
-- Upstream (not referenced) vertices come first:
--
-- >>> let !gr = buildSG 5 $ U.fromList ([(0, 1), (0, 2), (2, 3)] :: [(Int, Int)])
-- >>> topSortSG gr
-- [4,0,2,3,1]
topSortSG :: SparseGraph w -> [Vertex]
topSortSG gr@SparseGraph {..} = runST $ do
  !vis <- UM.replicate nVertsSG False

  let dfsM !acc !v = do
        UM.unsafeRead vis v >>= \case
          True -> pure acc
          False -> do
            UM.unsafeWrite vis v True
            !vs <- U.filterM (fmap not . UM.unsafeRead vis) $ gr `adj` v
            -- Create postorder output:
            (v :) <$> U.foldM' dfsM acc vs

  U.foldM' dfsM [] (U.generate nVertsSG id)

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

-- | \(O(V+E)\) Partial running of `topSccSG` over topologically sorted vertices, but for some connected components
-- only.
downScc1SG :: forall w m. (PrimMonad m) => SparseGraph w -> UM.MVector (PrimState m) Bool -> Vertex -> m [Vertex]
downScc1SG !gr' !vis !v0 = do
  flip fix ([], v0) $ \loop (!acc, !v) -> do
    UM.unsafeRead vis v >>= \case
      True -> pure acc
      False -> do
        UM.unsafeWrite vis v True
        !vs <- U.filterM (fmap not . UM.unsafeRead vis) $ gr' `adj` v
        -- Create preorder output:
        (v :) <$> U.foldM' (curry loop) acc vs

-- | \(O(V+E)\) Collectes strongly connected components, reverse topologically sorted (referenced
-- SCCs come first).
--
-- Downstream vertices come first, e.g., @[v1 == v2 == v3] <- [v4 == v5] <- [v6]@.
--
-- **TODO: Faster.**
downSccSG :: (U.Unbox w) => SparseGraph w -> [[Int]]
downSccSG gr = collectSccPreorderSG $ topSortSG gr
  where
    !gr' = revSG gr

    collectSccPreorderSG :: [Int] -> [[Int]]
    collectSccPreorderSG !topVerts = runST $ do
      !vis <- UM.replicate (nVertsSG gr) False
      filter (not . null) <$> mapM (downScc1SG gr' vis) topVerts

-- | \(O(V+E)\) Collectes strongly connected components, reverse topologically sorted
-- (not referenced SCCs come first).
topSccSG :: (U.Unbox w) => SparseGraph w -> [[Int]]
topSccSG = map reverse . downSccSG

-- * MST (Minimum Spanning Tree)

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

-- * Cycles

-- Note that the shortest cycle in directed graph can be found with Dijkstra
-- https://atcoder.jp/contests/abc376/submissions/58952028

-- | \(O(V+E)\) Finds a cycle in a directed graph and collects their vertices. Embed edge
-- information to the weight if necessary.
findCycleDirectedSG :: (U.Unbox w) => SparseGraph w -> Maybe (U.Vector (Vertex, Vertex, w))
findCycleDirectedSG = (`findCycleImplSG` True)

-- | \(O(V+E)\) Finds a cycle in an undirected graph and collects their vertices. Embed edge
-- information to the weight if necessary.
findCycleUndirectedSG :: (U.Unbox w) => SparseGraph w -> Maybe (U.Vector (Vertex, Vertex, w))
findCycleUndirectedSG gr = findCycleComplexSG gr <|> findCycleImplSG gr False

-- | \(O(E)\) (Internal) Auxiliary function to `fincCycleUndirectedSG. Detects self-loop
-- edges and multiple edges.
--
-- TODO: efficient implementation (findMap? and two of them in one loop?)
findCycleComplexSG :: (U.Unbox w) => SparseGraph w -> Maybe (U.Vector (Vertex, Vertex, w))
findCycleComplexSG gr = selfLoop <|> multiEdge
  where
    n = nVertsSG gr
    selfLoop = (V.!? 0) $ V.mapMaybe f (V.generate n id)
      where
        f v1 = (\(!_, !w) -> U.singleton (v1, v1, w)) <$> U.find ((== v1) . fst) (gr `adjW` v1)
    multiEdge = (V.!? 0) $ V.mapMaybe g (V.generate n id)
    g v1
      | U.length (gr `adj` v1) <= 1 = Nothing
      | Just delta <- U.findIndex id matches =
          let (!v2, !w12) = (gr `adjW` v1) G.! delta
              (!_, !w21) = (gr `adjW` v1) G.! (delta + 1)
           in Just $ U.fromListN 2 [(v1, v2, w12), (v2, v1, w21)]
      | otherwise = Nothing
      where
        -- FIXME: not always sorted
        matches = U.zipWith (==) (gr `adj` v1) (U.tail (gr `adj` v1))

-- | \(O(V+E)\) Finds a cycle in a directed or undirected graph and collects their vertices.
--
-- - @revEdgeIsCycle@: Reports reverse edge as a cycle on @True. It's set to @False@ for undirected
-- graphs where edges are duplicated.
--
-- <https://drken1215.hatenablog.com/entry/2023/05/20/200517>.
findCycleImplSG :: (U.Unbox w) => SparseGraph w -> Bool -> Maybe (U.Vector (Vertex, Vertex, w))
findCycleImplSG gr revEdgeIsCycle = runST $ do
  -- visited in-order/out-order
  visIn <- UM.replicate n False
  visOut <- UM.replicate n False

  -- (v, w) visited in-order
  history <- newRevBuffer n

  -- TODO: better way than `callCC`?
  res <- evalContT $ callCC $ \exit -> do
    let dfs parent v1 = do
          GM.write visIn v1 True
          U.forM_ (gr `adjW` v1) $ \(!v2, !w12) -> do
            when (revEdgeIsCycle || v2 /= parent) $ do
              bIn <- UM.read visIn v2
              when bIn $ do
                unlessM (UM.read visOut v2) $ do
                  -- cycle detected: break
                  pushFront history (v1, v2, w12)
                  exit $ Just v2
              unless bIn $ do
                pushFront history (v1, v2, w12)
                dfs v1 v2
                popFront_ history
          GM.write visOut v1 True

    U.forM_ (U.generate n id) $ \v -> do
      unlessM (UM.read visIn v) $ do
        clearBuffer history -- needed?
        dfs (-1) v

    pure Nothing

  -- when there's some cycling point:
  (`mapM` res) $ \v -> do
    his <- unsafeFreezeBuffer history
    let !i = fromJust $ U.findIndex (\(!v', !_, !_) -> v' == v) his
    -- REMARK: `reverse` is required because we're starting from the back to the front.
    -- REMARK: `force` is required to not refer to the memory owned by the u buffer
    pure . U.force . U.reverse $ U.take (i + 1) his
  where
    !n = nVertsSG gr
