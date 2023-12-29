{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

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
--   [ ] [C - Remembering the Days](https://atcoder.jp/contests/abc317/tasks/abc317_c)
-- - [ ] Come back to the first vertex
module Data.SuperGraph where

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
import Data.SparseGraph
import Data.Tree.Lca (LcaCache, ToParent (..))
import Data.Tuple.Extra (both)
import qualified Data.Vector.Generic as G
import Data.Vector.IxVector
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Stack (HasCallStack)
import ToyLib.Macro (dbgAssert)
import ToyLib.Prelude (add2, rangeU)

-- | Graph with search context.
class (Monad m) => SuperGraph s m w where
  -- | Collects the adjacent vertices, applying the filter.
  adjS :: s m w -> Vertex -> m (U.Vector Vertex)

  -- | Collects the adjacent vertices with the edge weights, applying the filter.
  adjWS :: s m w -> Vertex -> m (U.Vector (Vertex, w))

  -- | Marks the vertex as visited.
  markS :: s m w -> Vertex -> (Vertex, w) -> m ()

  -- | Marks the previously marked vertex as not visited. See `markScopeS` as the specific use.
  unmarkS :: s m w -> Vertex -> (Vertex, w) -> m ()

  -- | Marks the vertex as visited, runs the procedure, then unmarks the vertex.
  {-# INLINE markScopeS #-}
  markScopeS :: s m w -> Vertex -> (Vertex, w) -> m () -> m ()
  markScopeS sgr !v1 (!v2, !dw2) f = do
    markS sgr v1 (v2, dw2)
    f
    unmarkS sgr v1 (v2, dw2)

----------------------------------------------------------------------------------------------------
-- `AdhocDistGraph`
----------------------------------------------------------------------------------------------------

-- | Adhoc graph with distance-based search context.
data AdhocDistGraph m w = AdhocDistGraph
  { -- | Initial weights for non-visited vertices.
    undefADG :: w,
    -- | Distance context.
    distADG :: UM.MVector (PrimState m) w,
    -- | Non-weightened graph representation as a function.
    adjADG :: Vertex -> U.Vector Vertex,
    -- | Weightened graph representation as a function.
    adjWADG :: Vertex -> U.Vector (Vertex, w),
    -- | Hook
    onMarkADG :: AdhocDistGraph m w -> Vertex -> (Vertex, w) -> m (),
    -- | Hook
    onUnmarkADG :: AdhocDistGraph m w -> Vertex -> (Vertex, w) -> m ()
  }

-- | Default value for `onMarkADG`
onMarkADGDefault :: (PrimMonad m) => AdhocDistGraph m w -> Vertex -> (Vertex, w) -> m ()
onMarkADGDefault _ _ _ = return ()

-- | Default value for `onUnmarkADG`
onUnmarkADGDefault :: (PrimMonad m) => AdhocDistGraph m w -> Vertex -> (Vertex, w) -> m ()
onUnmarkADGDefault _ _ _ = return ()

instance (Num w, Ord w, U.Unbox w, PrimMonad m) => SuperGraph AdhocDistGraph m w where
  {-# INLINE adjS #-}
  adjS AdhocDistGraph {..} = U.filterM (fmap (== undefADG) . UM.read distADG) . adjADG
  {-# INLINE adjWS #-}
  adjWS AdhocDistGraph {..} = U.filterM (fmap (== undefADG) . UM.read distADG . fst) . adjWADG
  {-# INLINE markS #-}
  markS adg@AdhocDistGraph {..} !v1 (!v2, !dw2) = do
    !w1 <- UM.read distADG v1
    onMarkADG adg v1 (v2, dw2)
    UM.write distADG v2 $! w1 + dw2
  {-# INLINE unmarkS #-}
  unmarkS adg@AdhocDistGraph {..} !v1 (!v2, !dw2) = do
    onUnmarkADG adg v1 (v2, dw2)
    UM.write distADG v2 undefADG

-- | Creates [`AdhocDistGraph`] from a non-weightend sparse graph with `1` as the weight.
newAdg :: (PrimMonad m, Num w, U.Unbox w) => SparseGraph i wgr -> w -> m (AdhocDistGraph m w)
newAdg gr@SparseGraph {..} undef = do
  !dist <- UM.replicate nVertsSG undef
  return $ AdhocDistGraph undef dist (gr `adj`) (U.map (,1) . (gr `adj`)) onMarkADGDefault onUnmarkADGDefault

-- | Creates [`AdhocDistGraph`] from a weightened sparse graph.
newAdgW :: (PrimMonad m, U.Unbox w) => SparseGraph i w -> w -> m (AdhocDistGraph m w)
newAdgW gr@SparseGraph {..} undef = do
  !dist <- UM.replicate nVertsSG undef
  return $ AdhocDistGraph undef dist (gr `adj`) (gr `adjW`) onMarkADGDefault onUnmarkADGDefault

newAdgWith ::
  (PrimMonad m, Num w, U.Unbox w) =>
  SparseGraph i w ->
  w ->
  (AdhocDistGraph m w -> Vertex -> (Vertex, w) -> m ()) ->
  (AdhocDistGraph m w -> Vertex -> (Vertex, w) -> m ()) ->
  m (AdhocDistGraph m w)
newAdgWith gr@SparseGraph {..} undef onMark onUnmark = do
  !dist <- UM.replicate nVertsSG undef
  return $ AdhocDistGraph undef dist (gr `adj`) (U.map (,1) . (gr `adj`)) onMark onUnmark

newAdgWithW ::
  (PrimMonad m, U.Unbox w) =>
  SparseGraph i w ->
  w ->
  (AdhocDistGraph m w -> Vertex -> (Vertex, w) -> m ()) ->
  (AdhocDistGraph m w -> Vertex -> (Vertex, w) -> m ()) ->
  m (AdhocDistGraph m w)
newAdgWithW gr@SparseGraph {..} undef onMark onUnmark = do
  !dist <- UM.replicate nVertsSG undef
  return $ AdhocDistGraph undef dist (gr `adj`) (gr `adjW`) onMark onUnmark

-- | /O(V+E)/ Runs depth-first search.
runSuperDfs :: (U.Unbox w, PrimMonad m, SuperGraph AdhocDistGraph m w) => AdhocDistGraph m w -> Vertex -> m ()
runSuperDfs sgr source = do
  flip fix source $ \loop !v1 -> do
    ((sgr `adjWS` v1) >>=) . U.mapM_ $ \(!v2, !dw2) -> do
      markS sgr v1 (v2, dw2)
      loop v2
  return ()

-- | /O(N!)/ Runs depth-first search. REMARK: Consider using @tspDP@ instead.
runSuperDfsEveryPath :: (U.Unbox w, PrimMonad m, SuperGraph AdhocDistGraph m w) => AdhocDistGraph m w -> Vertex -> m ()
runSuperDfsEveryPath sgr source = do
  -- UM.write (distADG sgr) source 0
  flip fix source $ \loop !v1 -> do
    ((sgr `adjWS` v1) >>=) . U.mapM_ $ \(!v2, !dw2) -> do
      markScopeS sgr v1 (v2, dw2) $ do
        loop v2

----------------------------------------------------------------------------------------------------
-- Concrete functions for `AdhocDistGraph`
----------------------------------------------------------------------------------------------------

-- | /O(V+E)/ Depth-first search over an unweightend sparse graph.
--
-- >>> ghci> let gr = buildSG (0 :: Int, 3 :: Int) $ U.fromList [(0, 1), (1, 2), (2, 3)]
-- >>> ghci> superDfsSG gr 0
-- [0,1,2,3]
superDfsSG :: (forall s. SuperGraph AdhocDistGraph (ST s) w) => SparseGraph Int w -> Vertex -> U.Vector Int
superDfsSG gr source = U.create $ do
  !sgr <- newAdg gr (-1 :: Int)
  UM.write (distADG sgr) source 0
  runSuperDfs sgr source
  return $ distADG sgr

-- | /O(V+E)/ Depth-first search over a weightend graph.
--
-- >>> ghci> let gr = buildWSG (0 :: Int, 3 :: Int) $ U.fromList [(0, 1, 1 :: Int), (1, 2, 1), (2, 3, 1)]
-- >>> ghci> superWDfs gr 0
-- [0,1,2,3]
superDfsWSG :: (Num w, U.Unbox w, forall s. SuperGraph AdhocDistGraph (ST s) w) => SparseGraph Int w -> w -> Vertex -> U.Vector w
superDfsWSG gr undef source = U.create $ do
  !sgr <- newAdgW gr undef
  UM.write (distADG sgr) source 0
  runSuperDfs sgr source
  return $ distADG sgr

----------------------------------------------------------------------------------------------------
-- `AdhocVisGraph`
----------------------------------------------------------------------------------------------------

-- | Adhoc graph with visibility-based search context.
data AdhocVisGraph m = AdhocVisGraph
  { visAVG :: UM.MVector (PrimState m) Bool,
    -- | Retrieves neighbors without applying the filter.
    adjAVG :: Vertex -> U.Vector Vertex
  }
