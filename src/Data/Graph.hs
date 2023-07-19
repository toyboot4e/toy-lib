{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | I'm using @Array Int [Int]@ as a primary `Graph` data storage.
module Data.Graph where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.ST
import Data.Array.IArray
import Data.Array.IO
import Data.Array.ST
import Data.Array.Unboxed (UArray)
import qualified Data.Heap as H
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (foldl')
import Data.Maybe
import qualified Data.Sequence as Seq
import Data.Tuple.Extra (both)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import ToyLib.Prelude (add2, foldForM)

-- | Adjacency list representation of a graph with node type parameter `a`.
type Graph a = Array Int [a]

-- | Vertex index. Starts with zero.
type Vertex = Int

-- | Weighted `Graph`. @Entry priority payload@.
type WGraph a = Array Int [H.Entry a Vertex]

-- | Collects distances from one vertex to every other using BFS, returning a vector.
bfsVec :: Graph Int -> Int -> VU.Vector Int
bfsVec graph start = VU.create $ do
  let !undef = -1 :: Int
  !vis <- VUM.replicate (rangeSize $ bounds graph) undef

  let inner !depth !vs
        | IS.null vs = return ()
        | otherwise = do
            let vs' = IS.toList vs
            forM_ vs' $ \v -> do
              VUM.unsafeWrite vis v depth

            !next <- fmap (IS.fromList . concat) $ forM vs' $ \v -> do
              filterM (fmap (== undef) . VUM.unsafeRead vis) $ graph ! v

            inner (succ depth) next

  !_ <- inner (0 :: Int) (IS.singleton start)
  return vis

-- | BFS template for finding a shortest path from one vertex to another.
bfsPath :: Graph Int -> Int -> Int -> Maybe Int
bfsPath !graph !start !end = inner (-1) IS.empty (IS.singleton start)
  where
    inner :: Int -> IS.IntSet -> IS.IntSet -> Maybe Int
    inner !depth !vis !vs
      | IS.member end vis = Just depth
      | IS.null vs = Nothing
      | otherwise = inner (succ depth) vis' vs'
      where
        vis' = vis `IS.union` vs
        vs' = IS.fromList $! filter (`IS.notMember` vis') $! concatMap (graph !) (IS.toList vs)

-- | BFS template for collecting shortest paths from one vertex to every other.
bfsVerts :: Graph Int -> Int -> IM.IntMap Int
bfsVerts graph start = inner 0 IM.empty (IS.singleton start)
  where
    inner :: Int -> IM.IntMap Int -> IS.IntSet -> IM.IntMap Int
    inner !depth !vis !vs
      | IS.null vs = vis
      | otherwise = inner (succ depth) vis' vs'
      where
        vis' = IM.union vis $! IM.fromSet (const depth) vs
        vs' = IS.fromList $! filter (`IM.notMember` vis') $! concatMap (graph !) (IS.toList vs)

-- | BFS over grid. Not generalized (yet).
bfsGrid :: UArray (Int, Int) Char -> (Int, Int) -> UArray (Int, Int) Int
bfsGrid !grid !start = runSTUArray $ do
  let !bounds_ = bounds grid
  let (!_, !w) = both succ $! snd bounds_
  let isBlock !yx = grid ! yx == '#'

  let ix = index bounds_
  let unIndex !i = i `divMod` w
  let !undef = -1 :: Int

  !vis <- newArray bounds_ undef

  let nexts !yx0 = filter (\yx -> inRange bounds_ yx && not (isBlock yx)) $! map (add2 yx0) dyxs
        where
          dyxs = [(1, 0), (-1, 0), (0, 1), (0, -1)]

  let inner !depth !vs
        | IS.null vs = return ()
        | otherwise = do
            let yxs = map unIndex $! IS.toList vs

            forM_ yxs $ \yx -> do
              writeArray vis yx depth

            !vss <- forM yxs $ \yx -> do
              filterM (fmap (== undef) . readArray vis) $ nexts yx

            inner (succ depth) $! IS.fromList . map ix $! concat vss

  !_ <- inner (0 :: Int) (IS.singleton $ ix start)
  return vis

-- | Easy 01-BFS
--
-- = Typical problems
-- - [ABC 176 D](https://atcoder.jp/contests/abc176/tasks/abc176_d)
solve01BFS :: (Int, Int) -> UArray (Int, Int) Char -> UArray (Int, Int) Int
solve01BFS !start !grid = runSTUArray $ do
  !dp <- newArray (bounds grid) undef

  let popLoop Seq.Empty = return ()
      popLoop ((!v1, !d1) Seq.:<| seq0) = do
        !lastD <- readArray dp v1
        if lastD /= undef
          then popLoop seq0
          else do
            writeArray dp v1 d1
            popLoop <=< foldForM seq0 (grid `adjW` v1) $ \seq (!v2, !w2) -> do
              !d2 <- readArray dp v2
              if d2 /= undef
                then return seq
                else do
                  if w2 == 0
                    then return ((v2, d1) Seq.<| seq)
                    else return (seq Seq.|> (v2, succ d1))

  popLoop $ Seq.singleton (start, 0 :: Int)
  return dp
  where
    !undef = -1 :: Int
    adjW :: UArray (Int, Int) Char -> (Int, Int) -> [((Int, Int), Int)]
    adjW !grid !yx0 =
      let !adjs1 = map (,0 :: Int) $ filter ((&&) <$> inRange (bounds grid) <*> ((== '.') . (grid !))) $ map (add2 yx0) dir4
          !adjs2 = map (,1 :: Int) $ filter ((&&) <$> inRange (bounds grid) <*> ((== '.') . (grid !))) $ map (add2 yx0) bombs
       in adjs1 ++ adjs2
      where
        !dir4 = [(0, 1), (0, -1), (1, 0), (-1, 0)]
        !bombs = [(y, x) | y <- [-2 .. 2], x <- [-2 .. 2], abs x + abs y >= 2]

-- | Direction-based 01-BFS: <https://atcoder.jp/contests/typical90/tasks/typical90_aq>.
-- It's slow, but could be applied easily in certain situations.
--
-- = Typical problems
-- - [Typoical 043 - Maze Challenge with Lack of Sleep (★4)](https://atcoder.jp/contests/typical90/tasks/typical90_aq)
bfsGrid01 :: (Int, Int) -> UArray (Int, Int) Bool -> UArray (Int, Int, Int) Int
bfsGrid01 !start !isBlock = runSTUArray $ do
  -- dp ! (y, x, iDir). The third dimension is required!
  !dp <- newArray ((0, 0, 0), (pred h, pred w, pred 4)) undef

  forM_ [0 .. 3] $ \iDir -> do
    writeArray dp (fst start, snd start, iDir) 0

  let popLoop Seq.Empty = return ()
      popLoop (((!y0, !x0, !iDir0), d0) Seq.:<| seq0) =
        foldM step seq0 [0 .. 3] >>= popLoop
        where
          -- collects neighbors
          step !acc !iDir
            | not (inRange bounds_ (y, x)) || isBlock ! (y, x) = return acc
            | otherwise = do
                !lastD <- readArray dp (y, x, iDir)
                -- REMARK: we can come to the same point in the same direction in different ways:
                if lastD /= undef && lastD <= d'
                  then return acc
                  else do
                    writeArray dp (y, x, iDir) d'
                    if iDir == iDir0
                      then return $ nextItem Seq.<| acc
                      else return $ acc Seq.|> nextItem
            where
              (!y, !x) = add2 (y0, x0) (dyxs VU.! iDir)
              !d'
                | iDir == iDir0 = d0
                | otherwise = succ d0
              !nextItem = ((y, x, iDir), d')

  popLoop . Seq.fromList $ map (\iDir -> ((fst start, snd start, iDir), 0)) [0 .. 3]
  return dp
  where
    !undef = -1 :: Int
    !bounds_ = bounds isBlock
    (!h, !w) = both succ . snd $! bounds isBlock
    !dyxs = VU.fromList [(1, 0), (-1, 0), (0, 1), (0, -1)]

-- | DFS where all the reachable vertices from one vertex are collcetd.
components :: Graph Int -> Int -> IS.IntSet
components !graph !start = inner (IS.singleton start) start
  where
    inner vis v
      | null vs = vis'
      | otherwise = foldl' inner vis' vs
      where
        vs = filter (`IS.notMember` vis) $! graph ! v
        vis' = IS.union vis $! IS.fromList vs

-- | DFS for every path, specially for
-- [Typical 072](https://atcoder.jp/contests/typical90/tasks/typical90_bt>).
dfsEveryPathT072 :: UArray (Int, Int) Char -> (Int, Int) -> Int
dfsEveryPathT072 !gr !start
  | gr ! start == '#' = 0
dfsEveryPathT072 !gr !start = runST $ do
  !vis <- newArray (bounds gr) False :: ST s (STUArray s (Int, Int) Bool)

  let nexts v =
        filterM (fmap not . readArray vis)
          . filter ((&&) <$> inRange (bounds gr) <*> ((/= '#') . (gr !)))
          $! map (add2 v) [(0, 1), (0, -1), (1, 0), (-1, 0)]

  flip fix (0 :: Int, start) $ \loop (!d1, !v1) -> do
    -- start without marking `start` and visited:
    when (v1 /= start) $ do
      writeArray vis v1 True
    !v2s <- nexts v1
    !maxDistance <- fmap (foldl' max (0 :: Int)) . forM v2s $ \v2 -> do
      if v2 == start
        then return (succ d1)
        else loop (succ d1, v2)
    writeArray vis v1 False
    return maxDistance

-- | Checks a Simple Sndirected Graph and returns markings of cycle vertices.
-- TODO: Test if it works as expected.
cyclesSUG :: Array Vertex [Vertex] -> VU.Vector Bool
cyclesSUG !graph = VU.create $ do
  !degs <- VUM.replicate nVerts (0 :: Int)

  forM_ (assocs graph) $ \(!v1, !v2s) -> do
    forM_ v2s $ \v2 -> do
      VUM.modify degs succ v1
      VUM.modify degs succ v2

  !heap0 <- H.fromList <$> filterM (fmap (== 1) . VUM.read degs) [0 .. pred nVerts]
  !isCycleVert <- VUM.replicate nVerts True

  flip fix heap0 $ \loop !heap -> case H.uncons heap of
    Nothing -> return ()
    Just (!v1, !heap') -> do
      VUM.write degs 0 v1
      VUM.write isCycleVert v1 False
      loop <=< foldForM heap' (graph ! v1) $ \heap'' v2 -> do
        !deg <- VUM.read degs v2
        case deg of
          0 -> return heap''
          1 -> error "cycleSUD: degree 1 to degree 1?"
          2 -> do
            VUM.modify degs pred v2
            return $ H.insert v2 heap''
          _ -> do
            VUM.modify degs pred v2
            return heap''

  return isCycleVert
  where
    !nVerts = rangeSize (bounds graph)

-- | Dijkstra template that collects all the shortest distances from one vertex to every other.
-- Works for weightened graphs with positive edge capacities only.
--
-- Pro tip: Use reverse graph to collect cost from every other vertex to one (see `revDjAll`).
dj :: forall a. (Num a, Ord a) => WGraph a -> Int -> IM.IntMap a
dj !graph !start = inner (H.singleton $! H.Entry 0 start) IM.empty
  where
    merge :: H.Entry a Int -> H.Entry a Int -> H.Entry a Int
    merge (H.Entry !cost1 !_v1) (H.Entry !cost2 !v2) = H.Entry (cost1 + cost2) v2

    inner :: H.Heap (H.Entry a Int) -> IM.IntMap a -> IM.IntMap a
    inner !heap !vis
      | H.null heap = vis
      | IM.member v vis = inner heap' vis
      | otherwise = inner heap'' vis'
      where
        -- pop and visit it
        (entry@(H.Entry cost v), heap') = fromJust $! H.uncons heap
        vis' = IM.insert v cost vis

        -- push neighbors
        vs = map (merge entry) $! filter ((`IM.notMember` vis') . H.payload) $! graph ! v
        heap'' = foldl' (flip H.insert) heap' vs

-- | Runs Dijkstra's algorithm over a reversed graph of given graph.
revDj :: WGraph Int -> Int -> IM.IntMap Int
revDj !graph !start = dj (revWGraph graph) start

-- | Creates a reverse weightened graph.
revWGraph :: WGraph Int -> WGraph Int
revWGraph !graph = accumArray @Array (flip (:)) [] (bounds graph) $ concatMap revF $ assocs graph
  where
    revF (!v1, !v2s) = map (\(H.Entry !priority !v2) -> (v2, H.Entry priority v1)) v2s

-- | Dijkstra backed by a vector.
djVec :: forall a. (Num a, Ord a, VU.Unbox a) => WGraph a -> Int -> a -> VU.Vector a
djVec !graph !start !undef = VU.create $ do
  !vis <- VUM.replicate nVerts undef

  let inner !heap = case H.uncons heap of
        Nothing -> return ()
        Just (entry@(H.Entry cost v), heap') -> do
          !isNew <- (== undef) <$> VUM.read vis v
          if not isNew
            then inner heap'
            else do
              VUM.write vis v cost
              !vs <- map (merge entry) <$> filterM (fmap (== undef) . VUM.read vis . H.payload) (graph ! v)
              inner $! foldl' (flip H.insert) heap' vs

  inner (H.singleton $ H.Entry 0 start)
  return vis
  where
    !nVerts = rangeSize $! bounds graph

    merge :: H.Entry a Int -> H.Entry a Int -> H.Entry a Int
    merge (H.Entry !cost1 !_v1) (H.Entry !cost2 !v2) = H.Entry (cost1 + cost2) v2

-- | Runs Dijkstra's algorithm over a reversed graph of given graph.
revDjVec :: WGraph Int -> Int -> VU.Vector Int
revDjVec !graph !start = djVec (revWGraph graph) start (-1)
