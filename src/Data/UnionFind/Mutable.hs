{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
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
import qualified Data.Vector.Generic.Mutable as VGM
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import qualified Data.Vector.Unboxed.Mutable as VUM

-- {{{ Dense, mutable union-Find tree

-- | Dense, mutable union-find tree (originally by `@pel`)
newtype MUnionFind s = MUnionFind (VUM.MVector s MUFNode)

type IOUnionFind = MUnionFind RealWorld

type STUnionFind s = MUnionFind s

-- | `MUFChild parent | MUFRoot size`.
data MUFNode = MUFChild {-# UNPACK #-} !Int | MUFRoot {-# UNPACK #-} !Int

derivingUnbox
  "MUFNode"
  [t|MUFNode -> (Bool, Int)|]
  [|\case (MUFChild !x) -> (True, x); (MUFRoot !x) -> (False, x)|]
  [|\case (True, !x) -> MUFChild x; (False, !x) -> MUFRoot x|]

-- | Creates a new Union-Find tree of the given size.
{-# INLINE newMUF #-}
newMUF :: (PrimMonad m) => Int -> m (MUnionFind (PrimState m))
newMUF !n = MUnionFind <$> VUM.replicate n (MUFRoot 1)

-- | Returns the root node index.
{-# INLINE rootMUF #-}
rootMUF :: (PrimMonad m) => MUnionFind (PrimState m) -> Int -> m Int
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
groupsMUF :: (PrimMonad m) => MUnionFind (PrimState m) -> m IS.IntSet
groupsMUF uf@(MUnionFind !vec) = foldM step IS.empty [0 .. pred (VGM.length vec)]
  where
    step !is !i = do
      !root <- rootMUF uf i
      return $ IS.insert root is

-- | Checks if the two nodes are under the same root.
{-# INLINE sameMUF #-}
sameMUF :: (PrimMonad m) => MUnionFind (PrimState m) -> Int -> Int -> m Bool
sameMUF !uf !x !y = liftM2 (==) (rootMUF uf x) (rootMUF uf y)

-- | Just an internal helper.
_unwrapMUFRoot :: MUFNode -> Int
_unwrapMUFRoot (MUFRoot !s) = s
_unwrapMUFRoot (MUFChild !_) = error "tried to unwrap child as UF root"

-- | Unites two nodes.
{-# INLINE uniteMUF #-}
uniteMUF :: (PrimMonad m) => MUnionFind (PrimState m) -> Int -> Int -> m ()
uniteMUF uf@(MUnionFind !vec) !x !y = do
  !px <- rootMUF uf x
  !py <- rootMUF uf y
  when (px /= py) $ do
    !sx <- _unwrapMUFRoot <$> VUM.unsafeRead vec px
    !sy <- _unwrapMUFRoot <$> VUM.unsafeRead vec py
    -- NOTE(perf): union by rank (choose smaller one for root)
    let (!par, !chld) = if sx < sy then (px, py) else (py, px)
    VUM.unsafeWrite vec chld (MUFChild par)
    VUM.unsafeWrite vec par (MUFRoot (sx + sy))

-- | Returns the size of the root node, starting with `1`.
{-# INLINE sizeMUF #-}
sizeMUF :: (PrimMonad m) => MUnionFind (PrimState m) -> Int -> m Int
sizeMUF uf@(MUnionFind !vec) !x = do
  !px <- rootMUF uf x
  _unwrapMUFRoot <$> VUM.unsafeRead vec px

-- }}}
