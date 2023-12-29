{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | Graph search with context in the super graph.
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
class (U.Unbox w, PrimMonad m) => SuperGraph s m w where
  -- | Collect the adjacent vertices, applying the filter.
  adjS :: s m w -> Vertex -> m (U.Vector Vertex)

  -- | Collect the adjacent vertices with the edge weights, applying the filter.
  adjWS :: s m w -> Vertex -> m (U.Vector (Vertex, w))

  -- | Marks the vertex as visited.
  markS :: s m w -> Vertex -> (Vertex, w) -> m ()

  -- | Marks the vertex as not visited.
  unmarkS :: s m w -> Vertex -> m ()

  -- | Marks the vertex as visited, runs the procedure, then undos the marking.
  markScopeS :: s m w -> Vertex -> (Vertex, w) -> m () -> m ()

----------------------------------------------------------------------------------------------------
-- `AdhocDistGraph`
----------------------------------------------------------------------------------------------------

-- | Adhoc graph with distance-based search context.
data (PrimMonad m) => AdhocDistGraph m w = AdhocDistGraph
  { -- | Initial weights for non-visited vertices.
    undefADG :: w,
    -- | Distance context.
    distADG :: UM.MVector (PrimState m) w,
    -- | Non-weightened graph representation as a function.
    adjADG :: Vertex -> U.Vector Vertex,
    -- | Weightened graph representation as a function.
    adjWADG :: Vertex -> U.Vector (Vertex, w)
  }

-- | Creates [`AdhocDistGraph`] from a non-weightend sparse graph.
newAdg :: (PrimMonad m, Num w, U.Unbox w) => SparseGraph i wgr -> w -> m (AdhocDistGraph m w)
newAdg gr@SparseGraph {..} undef = do
  !dist <- UM.replicate nVertsSG undef
  return $ AdhocDistGraph undef dist (gr `adj`) (U.map (,1) . (gr `adj`))

-- | Creates [`AdhocDistGraph`] from a weightened sparse graph.
newAdgW :: (PrimMonad m, U.Unbox w) => SparseGraph i w -> w -> m (AdhocDistGraph m w)
newAdgW gr@SparseGraph {..} undef = do
  !dist <- UM.replicate nVertsSG undef
  return $ AdhocDistGraph undef dist (gr `adj`) (gr `adjW`)

-- TODO: why can't use `PrimState m` here?
instance (PrimMonad m) => SuperGraph AdhocDistGraph m Int where
  adjS AdhocDistGraph {..} = U.filterM (fmap (== undefADG) . UM.read distADG) . adjADG
  adjWS AdhocDistGraph {..} = U.filterM (fmap (== undefADG) . UM.read distADG . fst) . adjWADG
  markS AdhocDistGraph {..} !v1 (!v2, !dw2) = do
    !w1 <- UM.read distADG v1
    UM.write distADG v2 $! w1 + dw2
  unmarkS AdhocDistGraph {..} !v = do
    UM.write distADG v undefADG
  markScopeS st@AdhocDistGraph {..} !v1 (!v2, !dw2) f = do
    markS st v1 (v2, dw2)
    f
    UM.write distADG v2 undefADG

-- | /O(V+E)/ Depth-first search over an unweightend sparse graph.
--
-- >>> ghci> let gr = buildSG (0 :: Int, 3 :: Int) $ U.fromList [(0, 1), (1, 2), (2, 3)]
-- >>> ghci> superDfsSG gr 0
-- [0,1,2,3]
superDfsSG :: (forall s. SuperGraph AdhocDistGraph (ST s) w) => SparseGraph Int w -> Vertex -> U.Vector Int
superDfsSG gr source = U.create $ do
  !sgr <- newAdg gr (-1 :: Int)
  runSuperDfs sgr source
  return $ distADG sgr

-- | /O(V+E)/ Depth-first search over a weightend graph.
--
-- >>> ghci> let gr = buildWSG (0 :: Int, 3 :: Int) $ U.fromList [(0, 1, 1 :: Int), (1, 2, 1), (2, 3, 1)]
-- >>> ghci> superWDfs gr 0
-- [0,1,2,3]
superDfsWSG :: (Num w, forall s. SuperGraph AdhocDistGraph (ST s) w) => SparseGraph Int w -> w -> Vertex -> U.Vector w
superDfsWSG gr undef source = U.create $ do
  !sgr <- newAdgW gr undef
  runSuperDfs sgr source
  return $ distADG sgr

runSuperDfs :: (Num w, SuperGraph AdhocDistGraph m w) => AdhocDistGraph m w -> Vertex -> m ()
runSuperDfs gr source = do
  UM.write (distADG gr) source 0
  flip fix source $ \loop !v1 -> do
    ((gr `adjWS` v1) >>=) . U.mapM_ $ \(!v2, !dw2) -> do
      markS gr v1 (v2, dw2)
      loop v2
  return ()

----------------------------------------------------------------------------------------------------
-- `AdhocVisGraph`
----------------------------------------------------------------------------------------------------

-- | Adhoc graph with visibility-based search context.
data (PrimMonad m) => AdhocVisGraph m = AdhocVisGraph
  { visAVG :: UM.MVector (PrimState m) Bool,
    -- | Retrieves neighbors without applying the filter.
    adjAVG :: Vertex -> U.Vector Vertex
  }
