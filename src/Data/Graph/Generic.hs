{-# LANGUAGE LambdaCase #-}

-- | Generic graph search.
module Data.Graph.Generic where

-- TODO: separate search module
-- TODO: `Sum`, `Max`, `Min` into one for the Dijkstra or selective DP

import Control.Monad (forM_, when)
import Control.Monad.Cont (callCC, evalContT)
import Control.Monad.Extra (unlessM, whenJustM, whenM)
import Control.Monad.Fix (fix)
import Control.Monad.ST (runST)
import Control.Monad.State.Class (gets, modify')
import Control.Monad.Trans.State.Strict (execState, execStateT)
import Data.BinaryHeap
import Data.Bool (bool)
import Data.Buffer
import Data.Foldable (for_)
import Data.Graph.Alias (Vertex)
import qualified Data.Heap as H
import qualified Data.IntMap as IM
import Data.Ix
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Data.Vector.IxVector
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Stack (HasCallStack)

-- * Graph search

-- | \(O(V+E)\) DFS that paints connnected components starting from a vertex.
genericComponentsOf :: (Vertex -> U.Vector Vertex) -> Int -> U.Vector Vertex -> U.Vector Vertex
genericComponentsOf gr nVerts sources = runST $ do
  vis <- UM.replicate nVerts False
  let dfs v1 = do
        GM.write vis v1 True
        U.forM_ (gr v1) $ \v2 -> do
          unlessM (GM.read vis v2) $ do
            dfs v2
  U.forM_ sources dfs
  U.findIndices id <$> U.unsafeFreeze vis

-- | \(O(V+E)\) DFS that paints connnected components. Returns @(vertexToComponentId, components)@.
-- Works on non-directed graphs only.
genericGrouping :: (Vertex -> U.Vector Vertex) -> Int -> (U.Vector Int, [[Vertex]])
genericGrouping gr n = runST $ do
  components <- UM.replicate n (-1 :: Int)
  iGroupRef <- UM.replicate 1 (0 :: Int)

  -- TODO: use unfoldrM
  groupVerts <- (\f -> U.foldM' f [] (U.generate n id)) $ \acc v -> do
    g <- GM.read components v
    if g /= -1
      then pure acc
      else do
        iGroup <- GM.unsafeRead iGroupRef 0
        GM.unsafeModify iGroupRef (+ 1) 0
        fmap (: acc) . (`execStateT` []) $ flip fix v $ \loop v1 -> do
          GM.write components v1 iGroup
          modify' (v1 :)
          U.forM_ (gr v1) $ \v2 -> do
            whenM ((== -1) <$> GM.read components v2) $ do
              loop v2

  (,groupVerts) <$> U.unsafeFreeze components

-- | \(O(V+E)\) Depth-first search.
genericDfs :: (U.Unbox w, Num w, Eq w) => (Vertex -> U.Vector (Vertex, w)) -> Int -> Vertex -> w -> U.Vector w
genericDfs !gr !nVerts !src !undefW = runST $ do
  !dists <- UM.replicate nVerts undefW
  -- !parent <- UM.replicate nVerts undef
  (\f -> fix f 0 src) $ \loop !depth v1 -> do
    GM.write dists v1 depth
    U.forM_ (gr v1) $ \(!v2, !dw) -> do
      !d <- GM.read dists v2
      when (d == undefW) $ do
        -- GM.write parent v2 v1
        loop (depth + dw) v2
  -- (,) <$> U.unsafeFreeze dists <*> U.unsafeFreeze parent
  U.unsafeFreeze dists

-- | \(O(N N!)\) Depth-first search that finds the longest path. Just a template!
genericDfsLongestPath :: forall w. (Num w, Ord w, U.Unbox w) => (Vertex -> U.Vector (Vertex, w)) -> Int -> Vertex -> w
genericDfsLongestPath !gr !n !source = runST $ do
  !vis <- UM.replicate n False

  flip fix (0 :: w, source) $ \loop (!d1, !v1) -> do
    -- let !_ = dbg (source, v1)
    GM.write vis v1 True
    !v2s <- U.filterM (fmap not . GM.read vis . fst) $ gr v1
    !maxDistance <- fmap (U.foldl' max d1) . U.forM v2s $ \(!v2, !w) -> do
      loop (d1 + w, v2)
    GM.write vis v1 False
    pure maxDistance

-- | \(O(V+E)\) Breadth-first search. Unreachable vertices are given distance of @-1@.
genericBfs :: (U.Unbox w, Num w, Eq w) => (Int -> U.Vector (Vertex, w)) -> Int -> Vertex -> w -> U.Vector w
genericBfs !gr !nVerts !source !undefW = U.create $ do
  !dist <- UM.replicate nVerts undefW
  !queue <- newBuffer nVerts

  pushBack queue source
  GM.unsafeWrite dist source 0

  -- procedural programming is great
  fix $ \loop -> do
    whenJustM (popFront queue) $ \v1 -> do
      !d1 <- GM.unsafeRead dist v1
      U.forM_ (gr v1) $ \(!v2, !dw) -> do
        !lastD <- GM.unsafeRead dist v2
        when (lastD == undefW) $ do
          GM.unsafeWrite dist v2 $! d1 + dw
          pushBack queue v2
      loop

  pure dist

-- | \(O(V+E)\) Breadth-first search with multiple starting points.
genericBfs' :: (U.Unbox w, Num w, Eq w) => (Vertex -> w -> U.Vector Vertex) -> Int -> w -> U.Vector Vertex -> U.Vector w
genericBfs' !gr !nVerts !undefW !sources = U.create $ do
  !dist <- UM.replicate nVerts undefW
  !queue <- newBuffer nVerts

  U.forM_ sources $ \src -> do
    pushBack queue src
    GM.unsafeWrite dist src 0

  -- procedural programming is great
  fix $ \loop -> do
    whenJustM (popFront queue) $ \v1 -> do
      !d1 <- GM.unsafeRead dist v1
      U.forM_ (gr v1 d1) $ \v2 -> do
        let !dw = 1
        !lastD <- GM.unsafeRead dist v2
        when (lastD == undefW) $ do
          GM.unsafeWrite dist v2 $! d1 + dw
          pushBack queue v2
      loop

  pure dist

-- | \(O(V+E)\) Breadth-first search. We don't need the edge weights in most cases, however, there
-- are some cases that requires it.
--
-- TODO: remove other generic BFS
genericBfs'' :: forall w. (U.Unbox w, Num w, Eq w) => (Vertex -> w -> U.Vector (Vertex, w)) -> Int -> w -> U.Vector (Vertex, w) -> U.Vector w
genericBfs'' !gr !nVerts !undefW !sources = U.create $ do
  dist <- UM.replicate @_ @w nVerts undefW
  queue <- newBuffer nVerts

  U.forM_ sources $ \(!src, !w0) -> do
    -- Duplicate inputs are removed here:
    lastD <- GM.unsafeRead dist src
    when (lastD == undefW) $ do
      GM.unsafeWrite dist src w0
      pushBack queue src

  fix $ \loop -> do
    whenJustM (popFront queue) $ \v1 -> do
      d1 <- GM.unsafeRead dist v1
      U.forM_ (gr v1 d1) $ \(!v2, !dw) -> do
        lastD <- GM.unsafeRead dist v2
        when (lastD == undefW) $ do
          GM.unsafeWrite dist v2 $! d1 + dw
          pushBack queue v2
      loop

  pure dist

-- | \(O(V+E)\) Breadth-first search. The graph function returns a list.
genericBfsWithList :: forall w. (U.Unbox w, Num w, Eq w) => (Vertex -> w -> [(Vertex, w)]) -> Int -> w -> U.Vector (Vertex, w) -> U.Vector w
genericBfsWithList !gr !nVerts !undefW !sources = U.create $ do
  dist <- UM.replicate @_ @w nVerts undefW
  queue <- newBuffer nVerts

  U.forM_ sources $ \(!src, !w0) -> do
    -- Duplicate inputs are removed here:
    lastD <- GM.unsafeRead dist src
    when (lastD == undefW) $ do
      GM.unsafeWrite dist src w0
      pushBack queue src

  fix $ \loop -> do
    whenJustM (popFront queue) $ \v1 -> do
      d1 <- GM.unsafeRead dist v1
      for_ (gr v1 d1) $ \(!v2, !dw) -> do
        lastD <- GM.unsafeRead dist v2
        when (lastD == undefW) $ do
          GM.unsafeWrite dist v2 $! d1 + dw
          pushBack queue v2
      loop

  pure dist

-- | \(O(V+E)\) 01-BFS. Unreachable vertices are given distance of @-1@.
--
-- = Example
--
-- @
-- let !bnd3 = zero3 n n 4
-- let !res = genericBfs01 bnd3 grF (4 * n * n) source
--       where
--         grF (!y, !x, !iDir) = flip U.imapMaybe diag4 $ \i (!dy, !dx) -> do
--           let (!y', !x') = (y + dy, x + dx)
--           guard $ inRange (zero2 n n) (y', x') && gr @! (y', x') /= '#'
--           pure ((y', x', i), bool 1 0 (iDir == i))
--         source = U.generate 4 $ (sy, sx,)
-- @
--
-- = Typical problems
-- - [ABC 176 E - Wizard in Maze](https://atcoder.jp/contests/abc176/tasks/abc176_d)
-- - [ABC 246 E - Bishop 2](https://atcoder.jp/contests/abc246/tasks/abc246_e)
genericBfs01 :: (Ix i, U.Unbox i) => (i, i) -> (i -> U.Vector (i, Int)) -> Int -> U.Vector i -> IxUVector i Int
genericBfs01 !bndExt !gr !nEdges !sources = IxVector bndExt $ U.create $ do
  let !undef = -1 :: Int
  let !nVertsExt = rangeSize bndExt
  !vec <- IxVector bndExt <$> UM.replicate nVertsExt undef
  !deque <- newBufferAsDeque (nEdges + 1)

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
    whenJustM (popFront deque) $ \(!w, !vExt) -> do
      step w vExt
      loop

  pure $ vecIV vec

-- | \(O((E+V) \log {V})\) Dijkstra's algorithm.
--
-- Does pruning on heap entry pushing: <https://www.slideshare.net/yosupo/ss-46612984> P15
--
-- Note that longest Dijkstra can be implemented by just using a max heap.
genericDj :: forall w. (U.Unbox w, Num w, Ord w) => (Vertex -> U.Vector (Vertex, w)) -> Int -> Int -> w -> U.Vector Vertex -> U.Vector w
genericDj !gr !nVerts !nEdges !undef !vs0 = U.create $ do
  !dist <- UM.replicate nVerts undef
  !heap <- newMinBH (nEdges + 1)
  -- !last <- UM.replicate nVerts (-1 :: Vertex)

  U.forM_ vs0 $ \v -> do
    GM.write dist v 0
    insertBH heap (0, v)

  fix $ \loop ->
    deleteMaybeBH heap >>= \case
      Nothing -> pure ()
      Just (!w1, !v1) -> do
        !newVisit <- (== w1) <$> GM.read dist v1
        when newVisit $ do
          U.forM_ (gr v1) $ \(!v2, !dw2) -> do
            !w2 <- GM.read dist v2
            let !w2' = merge w1 dw2
            when (w2 == undef || w2' < w2) $ do
              GM.write dist v2 w2'
              -- GM.write last v2 v1
              insertBH heap (w2', v2)
        loop

  pure dist
  where
    {-# INLINE merge #-}
    merge :: w -> w -> w
    merge = (+)

-- | \(O((E+V) \log {V})\) Monoid-based more generic `genericDj`. TODO: Allow `Min w`.
dj' :: forall w. (U.Unbox w, Monoid w, Ord w) => (Int -> U.Vector (Int, w)) -> Int -> Int -> w -> U.Vector Vertex -> U.Vector w
dj' !gr !nVerts !nEdges !undef !vs0 = U.create $ do
  !dist <- UM.replicate nVerts undef
  !heap <- newMinBH (nEdges + 1)
  -- !last <- UM.replicate nVerts (-1 :: Vertex)

  U.forM_ vs0 $ \v -> do
    GM.write dist v mempty
    insertBH heap (mempty, v)

  fix $ \loop ->
    deleteMaybeBH heap >>= \case
      Nothing -> pure ()
      Just (!w1, !v1) -> do
        !newVisit <- (== w1) <$> GM.read dist v1
        when newVisit $ do
          U.forM_ (gr v1) $ \(!v2, !dw2) -> do
            !w2 <- GM.read dist v2
            let !w2' = w1 <> dw2
            when (w2 == undef || w2' < w2) $ do
              GM.write dist v2 w2'
              -- GM.write last v2 v1
              insertBH heap (w2', v2)
        loop

  pure dist

-- | \(O((E+V) \log {V})\) Dijkstra's algorithm with sparse heap.
--
-- TODO: test
genericSparseDj :: forall w. (U.Unbox w, Num w, Ord w) => (Int -> U.Vector (Int, w)) -> w -> U.Vector Vertex -> IM.IntMap w
genericSparseDj !gr !undef !vs0 = (`execState` IM.empty) $ do
  U.forM_ vs0 $ \v -> do
    modify' $ IM.insert v 0

  let !heap0 = H.fromList $ V.toList $ V.map (H.Entry 0) $ U.convert vs0
  flip fix heap0 $ \loop !heap ->
    case H.uncons heap of
      Nothing -> pure ()
      Just (H.Entry !w1 !v1, !heap') -> do
        !newVisit <- (\case Just w | w == w1 -> True; _ -> False) <$> gets (IM.lookup v1)
        !nextHeap <-
          if newVisit
            then do
              (\f -> U.foldM' f heap' (gr v1)) $ \h (!v2, !dw2) -> do
                !w2 <- fromMaybe undef <$> gets (IM.lookup v2)
                let !w2' = merge w1 dw2
                if w2 == undef || w2' < w2
                  then do
                    modify' $ IM.insert v2 w2'
                    pure $ H.insert (H.Entry w2' v2) h
                  else do
                    pure h
            else do
              pure heap'
        loop nextHeap
  where
    {-# INLINE merge #-}
    merge :: w -> w -> w
    merge = (+)

-- * Path restoration

-- | \(O(V!)\) Depth-first search for finding a path with length @L@. The complexity is for dense
-- graph and IT CAN BE MUCH LOWER on different sparse graphs.
genericDfsEveryPathL :: (Vertex -> U.Vector Vertex) -> Int -> Vertex -> Int -> Maybe (U.Vector Int)
genericDfsEveryPathL !gr !nVerts !source !targetLen = runST $ do
  let !undef = -1 :: Int
  !dist <- UM.replicate nVerts undef

  !res <- evalContT $ callCC $ \exit -> do
    (\f -> fix f (0 :: Int) source) $ \loop d1 v1 -> do
      -- let !_ = dbg (v1, d1, targetLen)
      GM.write dist v1 d1
      when (d1 == targetLen - 1) $ do
        -- let !_ = dbg ("found!")
        exit True
      !v2s <- U.filterM (fmap (== undef) . GM.read dist) $ gr v1
      U.forM_ v2s $ \v2 -> do
        loop (d1 + 1) v2
      GM.write dist v1 undef
      pure False

  if res then Just <$> U.unsafeFreeze dist else pure Nothing

-- | \(O(V+E)\) depth-first search. Returns a vector of parents. The source vertex or unrechable
-- vertices are given `-1` as their parent. Note that it doesn't return the shortest path.
genericDfsTree :: (U.Unbox w, Num w) => (Vertex -> U.Vector (Vertex, w)) -> Int -> Vertex -> w -> (U.Vector w, U.Vector Vertex)
genericDfsTree !gr !n !source !undefW = runST $ do
  let !undef = (-1 :: Int)
  !dist <- UM.replicate n undefW
  !prev <- UM.replicate n undef
  !queue <- newBuffer n

  pushBack queue source
  GM.unsafeWrite dist source 0
  -- be sure to not overwrite the parent of the source vertex (it has to be `-1`!)

  fix $ \loop -> do
    whenJustM (popFront queue) $ \v1 -> do
      !d1 <- GM.unsafeRead dist v1
      U.forM_ (gr v1) $ \(!v2, !dw) -> do
        !p <- GM.unsafeRead prev v2
        -- REMARK: Be sure to keep the source vertex's parent as `-1`:
        when (p == undef && v2 /= source) $ do
          GM.unsafeWrite prev v2 v1
          GM.unsafeWrite dist v2 $! d1 + dw
          pushBack queue v2
      loop

  (,) <$> U.unsafeFreeze dist <*> U.unsafeFreeze prev

-- | \(O(V+E)\) breadth-first search. Returns a vector of parents. The source vertex or unrechable
-- vertices are given `-1` as their parent.
genericBfsTree :: (U.Unbox w, Num w, Eq w) => (Vertex -> U.Vector (Vertex, w)) -> Int -> Vertex -> w -> (U.Vector w, U.Vector Vertex)
genericBfsTree !gr !n !source !undefW = runST $ do
  !dist <- UM.replicate n undefW
  !prev <- UM.replicate n (-1 :: Vertex)
  !queue <- newBuffer n

  pushBack queue source
  GM.unsafeWrite dist source 0
  -- be sure to not overwrite the parent of the source vertex (it has to be `-1`!)

  fix $ \loop -> do
    whenJustM (popFront queue) $ \v1 -> do
      !d1 <- GM.read dist v1
      U.forM_ (gr v1) $ \(!v2, !dw) -> do
        !d2 <- GM.read dist v2
        when (d2 == undefW) $ do
          GM.write prev v2 v1
          GM.write dist v2 $! d1 + dw
          pushBack queue v2
      loop

  (,) <$> U.unsafeFreeze dist <*> U.unsafeFreeze prev

-- | \(O((E+V) \log V)\) Dijkstra's algorithm with path restoration information.
genericDjTree :: forall w. (U.Unbox w, Num w, Ord w) => (Vertex -> U.Vector (Vertex, w)) -> Int -> Int -> w -> U.Vector Vertex -> (U.Vector w, U.Vector Vertex)
genericDjTree !gr !nVerts !nEdges !undef !vs0 = runST $ do
  !dist <- UM.replicate nVerts undef
  !heap <- newMinBH (nEdges + 1)
  !parents <- UM.replicate nVerts (-1 :: Vertex)

  U.forM_ vs0 $ \v -> do
    GM.write dist v 0
    insertBH heap (0, v)

  fix $ \loop ->
    deleteMaybeBH heap >>= \case
      Nothing -> pure ()
      Just (!w1, !v1) -> do
        !newVisit <- (== w1) <$> GM.read dist v1
        when newVisit $ do
          U.forM_ (gr v1) $ \(!v2, !dw2) -> do
            !w2 <- GM.read dist v2
            let !w2' = merge w1 dw2
            when (w2 == undef || w2' < w2) $ do
              GM.write dist v2 w2'
              GM.write parents v2 v1
              insertBH heap (w2', v2)
        loop

  (,) <$> U.unsafeFreeze dist <*> U.unsafeFreeze parents
  where
    {-# INLINE merge #-}
    merge :: w -> w -> w
    merge = (+)

-- | \(O(V)\) Given a vector of vertex parents, restores path from the source to a sink.
restorePath :: (HasCallStack) => U.Vector Vertex -> Vertex -> U.Vector Vertex
restorePath !toParent !sink =
  restoreAnyPath
    ( \v -> case toParent G.! v of
        (-1) -> Nothing
        p -> Just p
    )
    sink

-- | \(O(V)\) `restorePath` for any dimensional vertices.
--
-- TODO: restore without reverse?
{-# INLINE restoreAnyPath #-}
restoreAnyPath :: forall a. (U.Unbox a) => (a -> Maybe a) -> a -> U.Vector a
restoreAnyPath !toParent !sink = U.reverse $ U.unfoldr f (sink, False)
  where
    f :: (a, Bool) -> Maybe (a, (a, Bool))
    f (!v, !done)
      | done = Nothing
      | otherwise = case toParent v of
          Nothing -> Just (v, (v, True))
          Just p -> Just (v, (p, False))

-- * Tree

-- | Vistor-based DFS for query processing with persistent data structure with minimum garbage
-- collectin. Be sure to not make a bi-directed graph.
--
-- = Typical problems
-- - [Persistente Queue](https://judge.yosupo.jp/problem/persistent_queue)
-- - [Persistent Unionfind](https://judge.yosupo.jp/problem/persistent_unionfind)
runPersistentDfs ::
  (HasCallStack, Monad m, U.Unbox w) =>
  (Vertex -> U.Vector (Vertex, w)) ->
  Vertex ->
  a ->
  ((HasCallStack) => a -> Vertex -> Vertex -> w -> m a) ->
  m ()
runPersistentDfs gr start acc0 process = inner start acc0
  where
    inner u acc = do
      U.forM_ (gr u) $ \(!v, !w) -> do
        !acc' <- process acc u v w
        inner v acc'

-- * Misc

-- | \(O(V^3)\) Floyd-Warshall algorith. It uses `max` as relax operator and the second argument is
-- usually like @maxBound `div` 2@.
--
-- It's strict about path connection and invalid paths are ignored.
--
-- NOTE: @maxBound .>>. 1@ is the propert value for @undef@, but now it can be any value outside
-- the weight's domain.
--
-- = Dynamically add edges (\(O(N^2)\))
--
-- [ABC 375 F - Road Blocked](https://atcoder.jp/contests/abc375/tasks/abc375_f).
--
-- - P11 of https://img.atcoder.jp/arc035/editorial.pdf
-- - Line 92 of https://atcoder.jp/contests/abc375/submissions/58722806
{-# INLINE distsNN #-}
distsNN :: (U.Unbox w, Num w, Ord w) => Int -> w -> U.Vector (Int, Int, w) -> IxUVector (Int, Int) w
distsNN !nVerts !undef !wEdges = IxVector bnd $ U.create $ do
  !vec <- UM.replicate (nVerts * nVerts) undef

  -- diagonals (self to self)
  forM_ [0 .. nVerts - 1] $ \v -> do
    GM.write vec (index bnd (v, v)) 0

  -- initial walks
  U.forM_ wEdges $ \(!v1, !v2, !w) -> do
    let !i = index bnd (v1, v2)
    w0 <- GM.exchange vec i w
    -- consider multiple edges
    when (w0 /= undef && w0 < w) $ do
      GM.write vec (index bnd (v1, v2)) w0

  -- multiple walks (O(N^3))
  forM_ [0 .. nVerts - 1] $ \k -> do
    forM_ [0 .. nVerts - 1] $ \i -> do
      forM_ [0 .. nVerts - 1] $ \j -> do
        !x1 <- GM.read vec (index bnd (i, j))
        !x2 <- do
          !tmp1 <- GM.read vec (index bnd (i, k))
          !tmp2 <- GM.read vec (index bnd (k, j))
          pure $! bool (tmp1 + tmp2) undef $ tmp1 == undef || tmp2 == undef
        let !x'
              | x1 == undef = x2
              | x2 == undef = x1
              | otherwise = min x1 x2
        GM.write vec (index bnd (i, j)) x'

  pure vec
  where
    bnd :: ((Int, Int), (Int, Int))
    bnd = ((0, 0), (nVerts - 1, nVerts - 1))
