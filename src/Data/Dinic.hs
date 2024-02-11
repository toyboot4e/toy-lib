{-# LANGUAGE RecordWildCards #-}

-- | /O(V^2E)/ max flow algorithm (Dinic's algorithm). Heavily inspired by @cojna/iota@.
module Data.SparseGraph where

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
import Data.Functor.Identity
import Data.Graph (Vertex)
import qualified Data.Heap as H
import Data.Maybe
import Data.Ord (Down (..))
import Data.SemigroupAction
import Data.Tree.Lca (LcaCache, ToParent (..))
import Data.Tuple.Extra (both)
import Data.Unindex
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Data.Vector.IxVector
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Stack (HasCallStack)
import ToyLib.Macro (dbgAssert)
import ToyLib.Prelude (add2, rangeU)

-- | The state for Dinic's algorithm
--
-- = Internals
-- Internally the graph is stored in a CSR (compressed sparse row).
data Dinic s c = Dinic
  { nVertsD :: !Int,
    nEdgesD :: !Int,
    -- | Source vertex -> initial edge index
    offsetsD :: !(U.Vector Int),
    -- | Edge index -> destination vertex
    edgeDstD :: !(U.Vector Int),
    -- | Edge index -> reverse edge index
    edgeRevIndexD :: !(U.Vector Int),
    residualD :: !(UM.MVector s c)
    -- levelMF :: UM.MVector s Int
    -- iterMF :: UM.MVector s Int,
    -- queueMF :: Queue s Vertex,
  }

-- | /O(V^2E)/ max flow algorithm (Dinic's algorithm)
maxFlowD :: (U.Unbox c) => Int -> Int -> Int -> U.Vector (Vertex, c) -> Int
maxFlowD !nVerts !src !sink !edges = 0

-- | Builds `Dinic` from edges.
buildD :: forall c m. (U.Unbox c, Num c, PrimMonad m) => Int -> U.Vector (Vertex, Vertex, c) -> m (Dinic (PrimState m) c)
buildD !nVertsD !edges = do
  let !offsetsD = U.scanl' (+) (0 :: Int) $ U.create $ do
        !degs <- UM.replicate nVertsD (0 :: Int)
        U.forM_ edges $ \(!v1, !v2, !_) -> do
          UM.modify degs (+ 1) v1
          UM.modify degs (+ 1) v2
        return degs

  (!edgeDstD, !edgeRevIndexD, !residualD) <- do
    !edgeDst <- UM.replicate nEdgesD undef
    !edgeRevIndex <- UM.replicate nEdgesD undef
    !residual <- UM.replicate nEdgesD (0 :: c)

    !edgeCounter <- U.thaw offsetsD
    U.forM_ edges $ \(!v1, !v2, !cap) -> do
      -- consume the edge index
      !i1 <- UM.read edgeCounter v1
      !i2 <- UM.read edgeCounter v2
      UM.modify edgeCounter (+ 1) v1
      UM.modify edgeCounter (+ 1) v2
      -- record reverse edge index
      UM.write edgeRevIndex i1 i2
      UM.write edgeRevIndex i2 i1
      -- initialize edge capacities
      UM.write residual i1 cap

    (,,residual) <$> U.unsafeFreeze edgeDst <*> U.unsafeFreeze edgeRevIndex

  return Dinic {..}
  where
    !undef = -1 :: Int
    !nEdgesD = G.length edges

runD :: (U.Unbox c, Num c, Ord c, Bounded c, PrimMonad m) => Vertex -> Vertex -> Dinic (PrimState m) c -> m c
runD !src !sink !dinic@Dinic {..} = do
  error "TODO"
