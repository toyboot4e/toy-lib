{-# LANGUAGE LambdaCase #-}

-- TODO: Remove completely

-- | Generic graph search.
module Data.Graph.Generic where

import Control.Monad.Extra (unlessM)
import Control.Monad.ST (runST)
import Data.Graph.Alias (Vertex)
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Stack (HasCallStack)

-- * Graph search

-- | \(O(V+E)\) DFS that paints connnected components starting from a vertex.
genericComponentsOf :: Int -> (Vertex -> U.Vector Vertex) -> U.Vector Vertex -> U.Vector Vertex
genericComponentsOf nVerts gr sources = runST $ do
  vis <- UM.replicate nVerts False
  let dfs v1 = do
        GM.write vis v1 True
        U.forM_ (gr v1) $ \v2 -> do
          unlessM (GM.read vis v2) $ do
            dfs v2
  U.forM_ sources dfs
  U.findIndices id <$> U.unsafeFreeze vis

-- -- | \(O(V+E)\) DFS that paints connnected components. Returns @(vertexToComponentId, components)@.
-- -- Works on non-directed graphs only.
-- genericGrouping :: (Vertex -> U.Vector Vertex) -> Int -> (U.Vector Int, [[Vertex]])
-- genericGrouping gr n = runST $ do
--   components <- UM.replicate n (-1 :: Int)
--   iGroupRef <- UM.replicate 1 (0 :: Int)
--
--   -- TODO: use unfoldrM
--   groupVerts <- (\f -> U.foldM' f [] (U.generate n id)) $ \acc v -> do
--     g <- GM.read components v
--     if g /= -1
--       then pure acc
--       else do
--         iGroup <- GM.unsafeRead iGroupRef 0
--         GM.unsafeModify iGroupRef (+ 1) 0
--         fmap (: acc) . (`execStateT` []) $ flip fix v $ \loop v1 -> do
--           GM.write components v1 iGroup
--           modify' (v1 :)
--           U.forM_ (gr v1) $ \v2 -> do
--             whenM ((== -1) <$> GM.read components v2) $ do
--               loop v2
--
--   (,groupVerts) <$> U.unsafeFreeze components

-- -- | \(O((E+V) \log {V})\) Dijkstra's algorithm with sparse heap.
-- --
-- -- TODO: test
-- genericSparseDj :: forall w. (U.Unbox w, Num w, Ord w) => (Int -> U.Vector (Int, w)) -> w -> U.Vector Vertex -> IM.IntMap w
-- genericSparseDj !gr !undef !vs0 = (`execState` IM.empty) $ do
--   U.forM_ vs0 $ \v -> do
--     modify' $ IM.insert v 0
--
--   let !heap0 = H.fromList $ V.toList $ V.map (H.Entry 0) $ U.convert vs0
--   flip fix heap0 $ \loop !heap ->
--     case H.uncons heap of
--       Nothing -> pure ()
--       Just (H.Entry !w1 !v1, !heap') -> do
--         !newVisit <- (\case Just w | w == w1 -> True; _ -> False) <$> gets (IM.lookup v1)
--         !nextHeap <-
--           if newVisit
--             then do
--               (\f -> U.foldM' f heap' (gr v1)) $ \h (!v2, !dw2) -> do
--                 !w2 <- fromMaybe undef <$> gets (IM.lookup v2)
--                 let !w2' = merge w1 dw2
--                 if w2 == undef || w2' < w2
--                   then do
--                     modify' $ IM.insert v2 w2'
--                     pure $ H.insert (H.Entry w2' v2) h
--                   else do
--                     pure h
--             else do
--               pure heap'
--         loop nextHeap
--   where
--     {-# INLINE merge #-}
--     merge :: w -> w -> w
--     merge = (+)

-- -- | \(O(V!)\) Depth-first search for finding a path with length @L@. The complexity is for dense
-- -- graph and IT CAN BE MUCH LOWER on different sparse graphs.
-- genericDfsEveryPathL :: (Vertex -> U.Vector Vertex) -> Int -> Vertex -> Int -> Maybe (U.Vector Int)
-- genericDfsEveryPathL !gr !nVerts !source !targetLen = runST $ do
--   let !undef = -1 :: Int
--   !dist <- UM.replicate nVerts undef
--
--   !res <- evalContT $ callCC $ \exit -> do
--     (\f -> fix f (0 :: Int) source) $ \loop d1 v1 -> do
--       -- let !_ = dbg (v1, d1, targetLen)
--       GM.write dist v1 d1
--       when (d1 == targetLen - 1) $ do
--         -- let !_ = dbg ("found!")
--         exit True
--       !v2s <- U.filterM (fmap (== undef) . GM.read dist) $ gr v1
--       U.forM_ v2s $ \v2 -> do
--         loop (d1 + 1) v2
--       GM.write dist v1 undef
--       pure False
--
--   if res then Just <$> U.unsafeFreeze dist else pure Nothing

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
