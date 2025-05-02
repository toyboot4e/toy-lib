{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

-- | A union-Find tree or a disjoint set.
--
-- = Typical problems
-- - [Typical 012 - Red Painting (★4)](https://atcoder.jp/contests/typical90/tasks/typical90_l)
module Data.UnionFind.Mutable where

import Control.Monad
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (RealWorld)
import qualified Data.IntMap as IM
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Stack (HasCallStack)

-- | Dense, mutable Union-Find tree (originally by `@pel`)
--
-- >>> stree <- newMUF 3
-- >>> sameMUF stree 0 2
-- False
-- >>> unifyMUF stree 0 2
-- True
-- >>> sameMUF stree 0 2
-- True
-- >>> unifyMUF stree 0 2
-- False
--
-- = See also
-- <https://algo-method.com/descriptions/132>
newtype MUnionFind s = MUnionFind (UM.MVector s MUFNode)

type IOUnionFind = MUnionFind RealWorld

type STUnionFind s = MUnionFind s

-- | @MUFChild parent | MUFRoot size@.
data MUFNode = MUFChild {-# UNPACK #-} !Int | MUFRoot {-# UNPACK #-} !Int
  deriving (Eq, Show)

instance U.IsoUnbox MUFNode (Bool, Int) where
  {-# INLINE toURepr #-}
  toURepr (MUFChild !x) = (True, x)
  toURepr (MUFRoot !x) = (False, x)
  {-# INLINE fromURepr #-}
  fromURepr (True, !x) = MUFChild x
  fromURepr (False, !x) = MUFRoot x

newtype instance U.MVector s MUFNode = MV_MUFNode (UM.MVector s (Bool, Int))

newtype instance U.Vector MUFNode = V_MUFNode (U.Vector (Bool, Int))

deriving via (MUFNode `U.As` (Bool, Int)) instance GM.MVector UM.MVector MUFNode

deriving via (MUFNode `U.As` (Bool, Int)) instance G.Vector U.Vector MUFNode

instance U.Unbox MUFNode

-- | \(O(N)\) Creates a new Union-Find tree of the given size.
{-# INLINE newMUF #-}
newMUF :: (PrimMonad m) => Int -> m (MUnionFind (PrimState m))
newMUF !n = MUnionFind <$> UM.replicate n (MUFRoot 1)

-- | \(O(\alpha(N))\) Returns the root node index.
{-# INLINE rootMUF #-}
rootMUF :: (HasCallStack, PrimMonad m) => MUnionFind (PrimState m) -> Int -> m Int
rootMUF uf@(MUnionFind !vec) i = do
  !node <- UM.unsafeRead vec i
  case node of
    MUFRoot _ -> pure i
    MUFChild p -> do
      !r <- rootMUF uf p
      -- NOTE(perf): path compression (move the queried node to just under the root, recursivelly)
      UM.unsafeWrite vec i (MUFChild r)
      pure r

-- | \(O(\alpha(N))\) Checks if the two nodes are under the same root.
{-# INLINE sameMUF #-}
sameMUF :: (HasCallStack, PrimMonad m) => MUnionFind (PrimState m) -> Int -> Int -> m Bool
sameMUF !uf !x !y = liftM2 (==) (rootMUF uf x) (rootMUF uf y)

-- | Just an internal helper.
_unwrapMUFRoot :: MUFNode -> Int
_unwrapMUFRoot (MUFRoot !s) = s
_unwrapMUFRoot (MUFChild !_) = error "tried to unwrap child as UF root"

-- | \(O(1)\) Unifies two nodes. Returns `True` when thry're newly unified.
{-# INLINE unifyMUF #-}
unifyMUF :: (HasCallStack, PrimMonad m) => MUnionFind (PrimState m) -> Int -> Int -> m Bool
unifyMUF uf@(MUnionFind !vec) !x !y = do
  !px <- rootMUF uf x
  !py <- rootMUF uf y
  when (px /= py) $ do
    !sx <- _unwrapMUFRoot <$> UM.unsafeRead vec px
    !sy <- _unwrapMUFRoot <$> UM.unsafeRead vec py
    -- NOTE(perf): Union by size (choose smaller one for root).
    -- Another, more proper optimization would be union by rank (depth).
    let (!par, !chld) = if sx < sy then (px, py) else (py, px)
    UM.unsafeWrite vec chld (MUFChild par)
    UM.unsafeWrite vec par (MUFRoot $! sx + sy)
  pure $ px /= py

-- | \(O(1)\)
{-# INLINE unifyMUF_ #-}
unifyMUF_ :: (HasCallStack, PrimMonad m) => MUnionFind (PrimState m) -> Int -> Int -> m ()
unifyMUF_ uf x y = void $ unifyMUF uf x y

-- | \(O(\alpha(N))\) Returns the size of the a node, starting with `1`.
{-# INLINE sizeMUF #-}
sizeMUF :: (HasCallStack, PrimMonad m) => MUnionFind (PrimState m) -> Int -> m Int
sizeMUF uf@(MUnionFind !vec) !x = do
  !px <- rootMUF uf x
  _unwrapMUFRoot <$> UM.unsafeRead vec px

-- | \(O(N)\)
{-# INLINE clearMUF #-}
clearMUF :: (PrimMonad m) => MUnionFind (PrimState m) -> m ()
clearMUF (MUnionFind !vec) = do
  UM.set vec (MUFRoot 1)

-- | \(O(N \alpha(N))\) Returns all root vertices.
{-# INLINE groupRootsMUF #-}
groupRootsMUF :: (HasCallStack, PrimMonad m) => MUnionFind (PrimState m) -> m (U.Vector Int)
groupRootsMUF uf@(MUnionFind !vec) = U.filterM (\x -> (== x) <$> rootMUF uf x) (U.generate (GM.length vec) id)

-- | \(O(N W)\) FIXME: too slow
groupsMUF :: (HasCallStack, PrimMonad m) => MUnionFind (PrimState m) -> m (IM.IntMap [Int])
groupsMUF uf@(MUnionFind !vec) = do
  rvs <- V.generateM (GM.length vec) (\v -> (,[v]) <$> rootMUF uf v)
  pure $ IM.fromListWith (flip (++)) $ V.toList rvs

groupsMUF2 :: (PrimMonad m) => MUnionFind (PrimState m) -> m (V.Vector (U.Vector Int))
groupsMUF2 dsu@(MUnionFind vec) = do
  let nDsu = UM.length vec
  groupSize <- UM.replicate nDsu (0 :: Int)
  leaders <- U.generateM nDsu $ \i -> do
    li <- rootMUF dsu i
    GM.modify groupSize (+ 1) li
    pure li
  result <- do
    groupSize' <- U.unsafeFreeze groupSize
    V.mapM UM.unsafeNew $ U.convert groupSize'
  U.iforM_ leaders $ \i li -> do
    i' <- subtract 1 <$> GM.read groupSize li
    GM.write (result G.! li) i' i
    GM.write groupSize li i'
  V.filter (not . U.null) <$> V.mapM U.unsafeFreeze result
