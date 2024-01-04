{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Graph search with context for every vertex in the super graph.
--
-- Tree search doesn't need contect per vertex and thus not the target.
-- But it would be great if we can use he same function for both trees and graphs.
--
-- = Common cases
--
-- == Search
-- - [ ] DFS
-- - [ ] BFS
-- - [ ] Dijkstra
--
-- == Search tree
-- - [ ] Search paths
--
-- == Search tree
-- - [ ] DFS tree
-- - [ ] BFS tree
-- - [ ] Dijkstra tree
-- - [ ] 01-BFS
--
-- == Niche cases
-- - [ ] Tree diameter
-- - [ ] Walk through every path
--   [C - Remembering the Days](https://atcoder.jp/contests/abc317/tasks/abc317_c)
-- - [ ] Come back to the first vertex
module Data.AdhocGraph () where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bool (bool)
import Data.Buffer
import Data.Graph (Vertex)
import qualified Data.Heap as H
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import ToyLib.Macro (dbgAssert)

----------------------------------------------------------------------------------------------------
-- `AdhocDistGraph`
----------------------------------------------------------------------------------------------------

-- | Adhoc graph with distance-based search context.
data AdhocGraph m w = AdhocGraph
  { -- | Initial weights for non-visited vertices.
    undefAG :: !w,
    -- | The number of vertices.
    nVertsAG :: !Int,
    -- | Visibility context.
    visAG :: !(UM.MVector (PrimState m) Bool),
    -- | Distance context.
    distAG :: !(UM.MVector (PrimState m) w),
    -- | Distance context.
    parentAG :: !(UM.MVector (PrimState m) Int),
    -- | Non-weightened graph representation as a function.
    adjAG :: !(Vertex -> U.Vector Vertex),
    -- | Weightened graph representation as a function.
    adjWAG :: !(Vertex -> U.Vector (Vertex, w))
  }

-- | Adhoc graph search methods for weightened graphs.
--
-- Non-weighnted graphs need to be represented as a graph with weight `1`.
data AdhocMethod m w = AdhocMethod
  { -- | Marks a root vertex.
    markRootAM :: !(AdhocGraph m w -> Vertex -> w -> m ()),
    -- | Marks a child vertex.
    markAM :: !(AdhocGraph m w -> Vertex -> (Vertex, w) -> m ()),
    -- | Unmarks a child vertex.
    unmarkAM :: !(AdhocGraph m w -> Vertex -> (Vertex, w) -> m ()),
    -- | Filter for adjacent vertices on DFS.
    wfilterAM :: !(AdhocGraph m w -> Vertex -> (Vertex, w) -> m Bool)
  }

defaultMarkRootAM :: (PrimMonad m, U.Unbox w) => AdhocGraph m w -> Vertex -> w -> m ()
defaultMarkRootAM ag v w = do
  UM.write (distAG ag) v w

defaultMarkAM :: (PrimMonad m, U.Unbox w, Eq w, Num w) => (AdhocGraph m w -> Vertex -> (Vertex, w) -> m ())
defaultMarkAM ag v1 (!v2, !dw2) = do
  w1 <- UM.read (distAG ag) v1
  let !_ = dbgAssert (w1 /= undefAG ag) $ "v1 has undefiend distance: " ++ show v1 ++ " -> " ++ show v2
  UM.write (distAG ag) v2 $! w1 + dw2

defaultUnmarkAM :: (PrimMonad m, U.Unbox w) => (AdhocGraph m w -> Vertex -> (Vertex, w) -> m ())
defaultUnmarkAM ag _ (!v2, !_) = do
  UM.write (distAG ag) v2 (undefAG ag)

defaultWFilterAM :: (PrimMonad m, U.Unbox w, Eq w) => (AdhocGraph m w -> Vertex -> (Vertex, w) -> m Bool)
defaultWFilterAM ag _ (!v2, !_) = do
  (== undefAG ag) <$> UM.read (distAG ag) v2

amDefault :: (PrimMonad m, U.Unbox w, Num w, Eq w) => AdhocMethod m w
amDefault =
  AdhocMethod
    { markRootAM = defaultMarkRootAM,
      markAM = defaultMarkAM,
      unmarkAM = defaultUnmarkAM,
      wfilterAM = defaultWFilterAM
    }

amDefaultInt :: (PrimMonad m) => AdhocMethod m Int
amDefaultInt = amDefault

data AdhocGraphArgs w = AdhocGraphArgs
  { -- | Initial weights for non-visited vertices.
    undefAGA :: !w,
    -- | The number of vertices.
    nVertsAGA :: !Int,
    -- | Distance context.
    distAGA :: !Bool,
    -- | Distance context.
    visAGA :: !Bool,
    -- | The capacity of the parents.
    parentAGA :: !Bool,
    -- | Non-weightened graph representation as a function.
    adjAGA :: !(Vertex -> U.Vector Vertex),
    -- | Weightened graph representation as a function.
    adjWAGA :: !(Vertex -> U.Vector (Vertex, w))
  }

agaDefaultUnit :: AdhocGraphArgs ()
agaDefaultUnit =
  AdhocGraphArgs
    { undefAGA = (),
      nVertsAGA = 0,
      distAGA = False,
      visAGA = False,
      parentAGA = False,
      adjAGA = const U.empty,
      adjWAGA = const U.empty
    }

agaDefaultInt :: AdhocGraphArgs Int
agaDefaultInt =
  AdhocGraphArgs
    { undefAGA = -1 :: Int,
      nVertsAGA = 0,
      distAGA = False,
      visAGA = True,
      parentAGA = False,
      adjAGA = const U.empty,
      adjWAGA = const U.empty
    }

newAg :: (PrimMonad m, U.Unbox w) => AdhocGraphArgs w -> m (AdhocGraph m w)
newAg AdhocGraphArgs {..} = do
  !vis <- UM.replicate (bool 0 nVertsAGA visAGA) False
  !dist <- UM.replicate (bool 0 nVertsAGA distAGA) undefAGA
  !parent <- UM.replicate (bool 0 nVertsAGA visAGA) (-1 :: Vertex)
  return $ AdhocGraph undefAGA nVertsAGA vis dist parent adjAGA adjWAGA

----------------------------------------------------------------------------------------------------
-- Template methods
----------------------------------------------------------------------------------------------------

{-# INLINE adjWFilterAM #-}
adjWFilterAM :: (PrimMonad m, U.Unbox w) => AdhocMethod m w -> AdhocGraph m w -> Vertex -> m (U.Vector (Vertex, w))
adjWFilterAM AdhocMethod {..} ag@AdhocGraph {..} !v1 = U.filterM (wfilterAM ag v1) $ adjWAG v1

-- | Marks the next vertex while the closure is running.
{-# INLINE markScopeAM #-}
markScopeAM :: (PrimMonad m) => AdhocMethod m w -> AdhocGraph m w -> Vertex -> (Vertex, w) -> m () -> m ()
markScopeAM AdhocMethod {..} !ag !v1 (!v2, !w2) !f = do
  markAM ag v1 (v2, w2)
  f
  unmarkAM ag v1 (v2, w2)

----------------------------------------------------------------------------------------------------
-- `adj` / `adjW` monadic filters
----------------------------------------------------------------------------------------------------

-- | Filter by `distAG`: equals to @undefAG@ or not.
{-# INLINE distFilterAG #-}
distFilterAG :: (PrimMonad m, U.Unbox w, Ord w) => AdhocGraph m w -> U.Vector Vertex -> m (U.Vector Vertex)
distFilterAG AdhocGraph {..} = U.filterM (fmap (== undefAG) . UM.read distAG)
  where
    !_ = dbgAssert (not (GM.null distAG)) "`distAG` is null"

-- | Filter by `distAG`: equals to @undefAG@ or not.
{-# INLINE distFilterWAG #-}
distFilterWAG :: (PrimMonad m, U.Unbox w, Ord w) => AdhocGraph m w -> U.Vector (Vertex, w) -> m (U.Vector (Vertex, w))
distFilterWAG AdhocGraph {..} = U.filterM (fmap (== undefAG) . UM.read distAG . fst)
  where
    !_ = dbgAssert (not (GM.null distAG)) "`distAG` is null"

-- | Filter by `visAG`
{-# INLINE visFilterAG #-}
visFilterAG :: (PrimMonad m) => AdhocGraph m w -> U.Vector Vertex -> m (U.Vector Vertex)
visFilterAG AdhocGraph {..} = U.filterM (fmap not . UM.read visAG)
  where
    !_ = dbgAssert (not (GM.null visAG)) "`visAG` is null"

----------------------------------------------------------------------------------------------------
-- `mark`
----------------------------------------------------------------------------------------------------

-- | Marks distance.
{-# INLINE markDistAG #-}
markDistAG :: (PrimMonad m, U.Unbox w, Num w) => AdhocGraph m w -> Vertex -> (Vertex, w) -> m ()
markDistAG ag@AdhocGraph {..} !v1 (!v2, !dw2) = do
  !w1 <- UM.read distAG v1
  UM.write distAG v2 $! w1 + dw2
  where
    !_ = dbgAssert (not (GM.null distAG)) "`distAG` is null"

-- | Marks visibility.
{-# INLINE markVisAG #-}
markVisAG :: (PrimMonad m, U.Unbox w, Num w) => AdhocGraph m w -> Vertex -> (Vertex, w) -> m ()
markVisAG ag@AdhocGraph {..} !v1 (!v2, !dw2) = do
  !w1 <- UM.read distAG v1
  UM.write distAG v2 $! w1 + dw2
  where
    !_ = dbgAssert (not (GM.null visAG)) "`visAG` is null"

-- | Unmarks distance.
{-# INLINE unmarkDistAG #-}
unmarkDistAG :: (PrimMonad m, U.Unbox w) => AdhocGraph m w -> Vertex -> (Vertex, w) -> m ()
unmarkDistAG AdhocGraph {..} !_v1 (!v2, !_dw2) = do
  UM.write distAG v2 undefAG
  where
    !_ = dbgAssert (not (GM.null distAG)) "`distAG` is null"

-- | Unmarks visibility.
{-# INLINE unmarkVisAG #-}
unmarkVisAG :: (PrimMonad m) => AdhocGraph m w -> Vertex -> Vertex -> m ()
unmarkVisAG AdhocGraph {..} !_ !v2 = do
  UM.write visAG v2 False
  where
    !_ = dbgAssert (not (GM.null visAG)) "`visAG` is null"

{-# INLINE markParentAG #-}
markParentAG :: (PrimMonad m) => AdhocGraph m w -> Vertex -> (Vertex, w) -> m ()
markParentAG AdhocGraph {..} !v1 (!v2, !_) = do
  UM.write parentAG v2 v1
  where
    !_ = dbgAssert (not (GM.null parentAG)) "`parentAG` is null"

----------------------------------------------------------------------------------------------------
-- DFS (single tree)
----------------------------------------------------------------------------------------------------

-- | For DFS and BFS.
{-# INLINE distAdjWAG #-}
distAdjWAG :: (PrimMonad m, U.Unbox w, Ord w) => AdhocGraph m w -> Vertex -> m (U.Vector (Vertex, w))
distAdjWAG ag@AdhocGraph {..} = distFilterWAG ag . adjWAG

-- | /O(V+E)/ Runs depth-first search.
dfsAG :: (PrimMonad m, U.Unbox w, Num w, Ord w) => AdhocGraph m w -> AdhocMethod m w -> Vertex -> m ()
dfsAG ag AdhocMethod {..} source = do
  markRootAM ag source 0
  flip fix source $ \loop !v1 -> do
    ((ag `distAdjWAG` v1) >>=) . U.mapM_ $ \(!v2, !dw2) -> do
      markAM ag v1 (v2, dw2)
      loop v2

-- TODO: tree dialect

----------------------------------------------------------------------------------------------------
-- DFS (every path)
----------------------------------------------------------------------------------------------------

-- | /O(N!)/ Runs depth-first search in every path. REMARK: Consider using @tspDP@ instead.
dfsAllAG :: (PrimMonad m, U.Unbox w, Num w) => AdhocGraph m w -> AdhocMethod m w -> Vertex -> m ()
dfsAllAG ag am@AdhocMethod {..} source = do
  markRootAM ag source 0
  dfsBackAllAG ag am source

-- | /O(N!)/ Runs depth-first search in every path, returning to the start.
-- REMARK: Consider using @tspDP@ instead.
dfsBackAllAG :: (PrimMonad m, U.Unbox w) => AdhocGraph m w -> AdhocMethod m w -> Vertex -> m ()
dfsBackAllAG ag am source = do
  -- markRootAM ag source 0
  flip fix source $ \loop !v1 -> do
    (adjWFilterAM am ag v1 >>=) . U.mapM_ $ \(!v2, !dw2) -> do
      markScopeAM am ag v1 (v2, dw2) $ do
        loop v2

----------------------------------------------------------------------------------------------------
-- BFS, 01-BFS, Dijkstra helpers
----------------------------------------------------------------------------------------------------

-- | /O(V+E)/ Runs breadth-first search.
bfsAG :: (PrimMonad m, U.Unbox w, Num w, Ord w) => AdhocGraph m w -> AdhocMethod m w -> Vertex -> m ()
bfsAG ag@AdhocGraph {..} AdhocMethod {..} source = do
  markRootAM ag source 0

  !queue <- newBufferAsDeque nVertsAG
  pushBack queue source

  fix $ \loop -> do
    popFront queue >>= \case
      Nothing -> return ()
      Just v1 -> do
        ((ag `distAdjWAG` v1) >>=) $ U.mapM_ $ \(!v2, !dw2) -> do
          markAM ag v1 (v2, dw2)
          pushBack queue v2
  where
    !_ = dbgAssert (not (GM.null distAG)) "`distAG` is null"

----------------------------------------------------------------------------------------------------
-- 01-BFS, Dijkstra
----------------------------------------------------------------------------------------------------

-- | For 01-BFS or Dijkstra.
{-# INLINE pruneAdjWAG #-}
pruneAdjWAG :: (PrimMonad m, U.Unbox w, Ord w, Num w) => AdhocGraph m w -> Vertex -> w -> m (U.Vector (Vertex, w))
pruneAdjWAG AdhocGraph {..} !v1 !w1 = U.filterM p $ adjWAG v1
  where
    !_ = dbgAssert (not (GM.null distAG)) "`distAG` is null"
    p (!v2, !dw2) = do
      !wReserved2 <- UM.read distAG v2
      let w2' = w1 + dw2
      return $ wReserved2 == undefAG || w2' < wReserved2

-- | 01-BFS. TODO: Super vertices?
bfs01AG :: (PrimMonad m, U.Unbox w, Ord w, Num w) => AdhocGraph m w -> AdhocMethod m w -> Vertex -> m ()
bfs01AG ag@AdhocGraph {..} AdhocMethod {..} source = do
  markRootAM ag source 0

  !queue <- newBufferAsDeque nVertsAG
  pushBack queue (source, 0)

  fix $ \loop -> do
    popFront queue >>= \case
      Nothing -> return ()
      Just (!v1, !w1) -> do
        !w1Reserved <- UM.read distAG v1
        when (w1 == w1Reserved) $ do
          (pruneAdjWAG ag v1 w1 >>=) . U.mapM_ $ \(!v2, !dw2) -> do
            markAM ag v1 (v2, dw2)
            !w2 <- GM.read distAG v2
            if dw2 == 0
              then pushFront queue (v2, w2)
              else pushBack queue (v2, w2)
        loop
  where
    !_ = dbgAssert (not (GM.null distAG)) "`distAG` is null"

-- | /O(V+E)/ Runs breadth-first search.
djAG :: (PrimMonad m, U.Unbox w, Ord w, Num w) => AdhocGraph m w -> AdhocMethod m w -> Vertex -> m ()
djAG ag@AdhocGraph {..} AdhocMethod {..} source = do
  UM.write distAG source 0

  let !heap0 = H.singleton $ H.Entry 0 source
  flip fix heap0 $ \loop !heap -> case H.uncons heap of
    Nothing -> return ()
    Just (H.Entry !w1 !v1, heap') -> do
      loop <=< ((pruneAdjWAG ag v1 w1 >>=) . (`U.foldM'` heap')) $ \h (!v2, !dw2) -> do
        markAM ag v1 (v2, dw2)
        !w2 <- GM.read distAG v2
        return $ H.insert (H.Entry w2 v2) h
  where
    !_ = dbgAssert (not (GM.null distAG)) "`distAG` is null"
