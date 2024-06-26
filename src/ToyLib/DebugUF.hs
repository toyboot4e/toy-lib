-- | Debug utilities
module ToyLib.DebugUF where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.UnionFind.Mutable
import qualified Data.Vector.Unboxed as U
import ToyLib.Debug (dbgVec)

-- FIXME: why such a redundant contraint is required?

-- | Shows the Union-Find vertices.
dbgUF :: (PrimMonad m, Show (U.Vector MUFNode)) => MUnionFind (PrimState m) -> m ()
dbgUF (MUnionFind vec) = dbgVec vec
