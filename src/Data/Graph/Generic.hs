{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Generic graph search.
module Data.Graph.Generic where

-- TODO: separate search module

import Control.Monad
import Control.Monad.Extra (unlessM)
import Control.Monad.Fix
import Control.Monad.ST
import Control.Monad.State.Class
import Control.Monad.Trans.State.Strict (State, StateT, evalState, evalStateT, execState, execStateT, runState, runStateT)
import Data.BinaryHeap
import Data.Bool (bool)
import Data.Buffer
import Data.Graph.Alias (Vertex)
import qualified Data.Heap as H
import qualified Data.IntMap as IM
import Data.Ix
import Data.Maybe
import qualified Data.Vector as V
import Data.Vector.IxVector
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- | \(O(V+E)\) DFS that paints connnected components starting from a vertex.
componentsOf :: (Vertex -> U.Vector Vertex) -> Int -> Vertex -> U.Vector Vertex
componentsOf gr nVerts start = runST $ do
  vis <- UM.replicate nVerts False

  flip fix start $ \loop v1 -> do
    UM.write vis v1 True
    U.forM_ (gr v1) $ \v2 -> do
      unlessM (UM.read vis v2) $ do
        loop v2

  U.findIndices id <$> U.unsafeFreeze vis

-- | \(O(V+E)\) breadth-first search. Unreachable vertices are given distance of @-1@.
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
--           return ((y', x', i), bool 1 0 (iDir == i))
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
    popFront deque >>= \case
      Nothing -> return ()
      Just (!w, !vExt) -> do
        step w vExt
        loop

  return $ vecIV vec

-- | \(O((E+V) \log {V})\) Dijkstra's algorithm.
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
    deleteBH heap >>= \case
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

-- | More generic `genericDj`. TODO: Replace
dj' :: forall w. (U.Unbox w, Monoid w, Ord w) => (Int -> U.Vector (Int, w)) -> Int -> Int -> w -> U.Vector Vertex -> U.Vector w
dj' !gr !nVerts !nEdges !undef !vs0 = U.create $ do
  !dist <- UM.replicate nVerts undef
  !heap <- newMinBinaryHeap (nEdges + 1)
  -- !last <- UM.replicate nVerts (-1 :: Vertex)

  U.forM_ vs0 $ \v -> do
    UM.write dist v mempty
    insertBH heap (mempty, v)

  fix $ \loop ->
    deleteBH heap >>= \case
      Nothing -> return ()
      Just (!w1, !v1) -> do
        !newVisit <- (== w1) <$> UM.read dist v1
        when newVisit $ do
          U.forM_ (gr v1) $ \(!v2, !dw2) -> do
            !w2 <- UM.read dist v2
            let !w2' = w1 <> dw2
            when (w2 == undef || w2' < w2) $ do
              UM.write dist v2 w2'
              -- UM.write last v2 v1
              insertBH heap (w2', v2)
        loop

  return dist

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
      Nothing -> return ()
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
-- Misc
----------------------------------------------------------------------------------------------------

-- | \(O(V^3)\) Floyd-Warshall algorith. It uses `max` as relax operator and the second argument is
-- usually like @maxBound `div` 2@.
--
-- It's strict about path connection and invalid paths are ignored.
--
-- REMARK: Use @maxBound@ for the @undef@ value.
distsNN :: (U.Unbox w, Num w, Ord w) => Int -> w -> U.Vector (Int, Int, w) -> IxUVector (Int, Int) w
distsNN !nVerts !undef !wEdges = IxVector bnd $ U.create $ do
  !vec <- UM.replicate (nVerts * nVerts) undef

  U.forM_ wEdges $ \(!v1, !v2, !w) -> do
    UM.write vec (index bnd (v1, v2)) w

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