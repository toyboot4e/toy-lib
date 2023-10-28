{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

-- | Union-find tree
--
-- = Typical problems
-- - [Typical 012 - Red Painting (★4)](https://atcoder.jp/contests/typical90/tasks/typical90_l)
module Data.UnionFind.Mutable where

import Control.Monad
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (RealWorld)
import qualified Data.IntSet as IS
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed.Base as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import GHC.Stack (HasCallStack)

-- {{{ Dense, mutable union-Find tree

-- | Dense, mutable union-find tree (originally by `@pel`)
--
-- >>> stree <- newMUF 3
-- >>> sameMUF stree 0 2
-- False
-- >>> uniteMUF stree 0 2
-- True
-- >>> sameMUF stree 0 2
-- True
-- >>> uniteMUF stree 0 2
-- False
newtype MUnionFind s = MUnionFind (VUM.MVector s MUFNode)

type IOUnionFind = MUnionFind RealWorld

type STUnionFind s = MUnionFind s

-- | `MUFChild parent | MUFRoot size`.
data MUFNode = MUFChild {-# UNPACK #-} !Int | MUFRoot {-# UNPACK #-} !Int

instance VU.IsoUnbox MUFNode (Bool, Int) where
  {-# INLINE toURepr #-}
  toURepr (MUFChild !x) = (True, x)
  toURepr (MUFRoot !x) = (False, x)
  {-# INLINE fromURepr #-}
  fromURepr (True, !x) = MUFChild x
  fromURepr (False, !x) = MUFRoot x

newtype instance VU.MVector s MUFNode = MV_MUFNode (VUM.MVector s (Bool, Int))

newtype instance VU.Vector MUFNode = V_MUFNode (VU.Vector (Bool, Int))

deriving via (MUFNode `VU.As` (Bool, Int)) instance VGM.MVector VUM.MVector MUFNode

deriving via (MUFNode `VU.As` (Bool, Int)) instance VG.Vector VU.Vector MUFNode

instance VU.Unbox MUFNode

-- | Creates a new Union-Find tree of the given size.
{-# INLINE newMUF #-}
newMUF :: (PrimMonad m) => Int -> m (MUnionFind (PrimState m))
newMUF !n = MUnionFind <$> VUM.replicate n (MUFRoot 1)

-- | Returns the root node index.
{-# INLINE rootMUF #-}
rootMUF :: (HasCallStack, PrimMonad m) => MUnionFind (PrimState m) -> Int -> m Int
rootMUF uf@(MUnionFind !vec) i = do
  !node <- VUM.unsafeRead vec i
  case node of
    MUFRoot _ -> return i
    MUFChild p -> do
      !r <- rootMUF uf p
      -- NOTE(perf): path compression (move the queried node to just under the root, recursivelly)
      VUM.unsafeWrite vec i (MUFChild r)
      return r

-- | Returns all root vertices.
{-# INLINE groupsMUF #-}
groupsMUF :: (HasCallStack, PrimMonad m) => MUnionFind (PrimState m) -> m IS.IntSet
groupsMUF uf@(MUnionFind !vec) = foldM step IS.empty [0 .. pred (VGM.length vec)]
  where
    step !is !i = do
      !root <- rootMUF uf i
      return $ IS.insert root is

-- | Checks if the two nodes are under the same root.
{-# INLINE sameMUF #-}
sameMUF :: (HasCallStack, PrimMonad m) => MUnionFind (PrimState m) -> Int -> Int -> m Bool
sameMUF !uf !x !y = liftM2 (==) (rootMUF uf x) (rootMUF uf y)

-- | Just an internal helper.
_unwrapMUFRoot :: MUFNode -> Int
_unwrapMUFRoot (MUFRoot !s) = s
_unwrapMUFRoot (MUFChild !_) = error "tried to unwrap child as UF root"

-- | Unites two nodes. Returns `True` when thry're newly united.
{-# INLINE uniteMUF #-}
uniteMUF :: (HasCallStack, PrimMonad m) => MUnionFind (PrimState m) -> Int -> Int -> m Bool
uniteMUF uf@(MUnionFind !vec) !x !y = do
  !px <- rootMUF uf x
  !py <- rootMUF uf y
  when (px /= py) $ do
    !sx <- _unwrapMUFRoot <$> VUM.unsafeRead vec px
    !sy <- _unwrapMUFRoot <$> VUM.unsafeRead vec py
    -- NOTE(perf): union by rank (choose smaller one for root)
    let (!par, !chld) = if sx < sy then (px, py) else (py, px)
    VUM.unsafeWrite vec chld (MUFChild par)
    VUM.unsafeWrite vec par (MUFRoot $! sx + sy)
  return $ px /= py

-- | Returns the size of the a node, starting with `1`.
{-# INLINE sizeMUF #-}
sizeMUF :: (HasCallStack, PrimMonad m) => MUnionFind (PrimState m) -> Int -> m Int
sizeMUF uf@(MUnionFind !vec) !x = do
  !px <- rootMUF uf x
  _unwrapMUFRoot <$> VUM.unsafeRead vec px

{-# INLINE clearMUF #-}
clearMUF :: (PrimMonad m) => MUnionFind (PrimState m) -> m ()
clearMUF (MUnionFind !vec) = do
  VUM.set vec (MUFRoot 1)

-- }}}
