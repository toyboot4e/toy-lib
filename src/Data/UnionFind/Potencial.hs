{-# LANGUAGE LambdaCase #-}

-- | Union-find tree under a differencial constraint system.
--
-- = Typical problems
-- - [ABC 328 F - Good Set Query](https://atcoder.jp/contests/abc328/tasks/abc328_f)
module Data.UnionFind.Potencial where

import Control.Monad
import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.IntMap as IM
import Data.UnionFind.Mutable (MUFNode (..), _unwrapMUFRoot)
import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- | Union-Find tree under a differencial constraint systems. Each vertex @v@ is given potencial
-- value @p(v)@ which is a relative value tested on `unifyPUF`.
--
-- Implementation based on: <https://qiita.com/drken/items/cce6fc5c579051e64fab>
data PUnionFind s a = PUnionFind
  { -- | Node data (@MUFParent size | MUFNode parent@).
    nodesPUF :: !(UM.MVector s MUFNode),
    -- | Diffierencial potencial of each vertex.
    potencialPUF :: !(UM.MVector s a)
  }

-- TODO: consider using `unsafe` when the performance is important

-- Fundamentals

-- | \(O(N)\) Creates a new union-find tree under a differencial-potencal system.
{-# INLINE newPUF #-}
newPUF :: forall m a. (PrimMonad m, Num a, U.Unbox a) => Int -> m (PUnionFind (PrimState m) a)
newPUF n = PUnionFind <$> UM.replicate n (MUFRoot 1) <*> UM.replicate n (0 :: a)

-- | \(O(\alpha(N))\) Returns root to the given vertex, after updating the internal differencial potencials.
{-# INLINE rootPUF #-}
rootPUF :: (PrimMonad m, Num a, U.Unbox a) => PUnionFind (PrimState m) a -> Int -> m Int
rootPUF uf = inner
  where
    inner v =
      UM.read (nodesPUF uf) v >>= \case
        MUFRoot _ -> return v
        MUFChild p -> do
          -- NOTE(perf): Path compression.
          -- Handle the nodes closer to the root first and move them onto just under the root
          !r <- inner p
          when (p /= r) $ do
            !pp <- UM.read (potencialPUF uf) p
            -- Move `v` to just under the root:
            UM.write (nodesPUF uf) v (MUFChild r)
            UM.modify (potencialPUF uf) (pp +) v
          return r

-- | \(O(\alpha(N))\) Unifies two nodes, managing their differencial potencial so that
-- @p(v1) - p(v2) = d@ holds. Returns `True` when thry're newly unified. Returns `False` if they're
-- already unified or when the unification didn't match the existing potencials.
{-# INLINE unifyPUF #-}
unifyPUF :: (PrimMonad m, Num a, Ord a, U.Unbox a) => PUnionFind (PrimState m) a -> Int -> Int -> a -> m Bool
unifyPUF !uf !v1 !v2 !dp = do
  !r1 <- rootPUF uf v1
  !r2 <- rootPUF uf v2
  if r1 == r2
    then return False
    else do
      -- NOTE(perf): Union by size (choose smaller one for root).
      -- Another, more proper optimization would be union by rank (depth).
      !size1 <- UM.read (potencialPUF uf) v1
      !size2 <- UM.read (potencialPUF uf) v2
      if size1 < size2
        then unifyPUF uf v2 v1 (-dp)
        else do
          -- Unify `r2` onto `r1`

          -- Update the size of `r1`
          !sz1 <- _unwrapMUFRoot <$> UM.read (nodesPUF uf) r1
          !sz2 <- _unwrapMUFRoot <$> UM.read (nodesPUF uf) r2
          UM.write (nodesPUF uf) r1 (MUFRoot (sz1 + sz2))

          -- p2 becomes p2' under r1 after unification.
          --     p1 - p2' = dp
          --      <=> p2' = p1 - dp   .. (1)
          --          p2' = pr2 + p2  .. (2)
          -- Threfore,
          --          pr2 = p1 - dp - p2
          !p1 <- UM.read (potencialPUF uf) v1
          !p2 <- UM.read (potencialPUF uf) v2
          let !pr2 = p1 - p2 - dp

          -- Move `r2` to just under `r1`
          UM.write (nodesPUF uf) r2 (MUFChild r1)
          UM.write (potencialPUF uf) r2 pr2

          return True

-- | `unifyPUF` with the return value discarded.
{-# INLINE unifyPUF_ #-}
unifyPUF_ :: (PrimMonad m, Num a, Ord a, U.Unbox a) => PUnionFind (PrimState m) a -> Int -> Int -> a -> m ()
unifyPUF_ !uf !v1 !v2 !dp = void $ unifyPUF uf v1 v2 dp

-- More API

-- | \(O(1)\) Returns the number of vertices belonging to the same group.
{-# INLINE sizePUF #-}
sizePUF :: (PrimMonad m, Num a, U.Unbox a) => PUnionFind (PrimState m) a -> Int -> m Int
sizePUF !uf !v = fmap _unwrapMUFRoot . UM.read (nodesPUF uf) =<< rootPUF uf v

-- | \(O(\alpha(N))\) Has the same root / belongs to the same group.
{-# INLINE samePUF #-}
samePUF :: (PrimMonad m, Num a, U.Unbox a) => PUnionFind (PrimState m) a -> Int -> Int -> m Bool
samePUF !uf !v1 !v2 = (==) <$> rootPUF uf v1 <*> rootPUF uf v2

-- | \(O(\alpha(N))\) Can be unified (or already unified) keeping the equiation @p(v1) - p(v2) = d@.
{-# INLINE canUnifyPUF #-}
canUnifyPUF :: (PrimMonad m, Num a, Eq a, U.Unbox a) => PUnionFind (PrimState m) a -> Int -> Int -> a -> m Bool
canUnifyPUF !uf !v1 !v2 !d = do
  !r1 <- rootPUF uf v1
  !r2 <- rootPUF uf v2
  !p1 <- UM.read (potencialPUF uf) v1
  !p2 <- UM.read (potencialPUF uf) v2
  return $ r1 /= r2 || p1 - p2 == d

-- | Returns @p(v)@: the potencial of the vertex in their group.
{-# INLINE potPUF #-}
potPUF :: (PrimMonad m, Num a, U.Unbox a) => PUnionFind (PrimState m) a -> Int -> m a
potPUF !uf !v1 = do
  -- Perform path compression
  void $ rootPUF uf v1
  UM.read (potencialPUF uf) v1

-- | \(O(\alpha(N))\) Returns @p(v1) - p(v2)@. Returns non-meaning value when the two vertices are
-- not cnnected.
{-# INLINE diffPUF #-}
diffPUF :: (PrimMonad m, Num a, U.Unbox a) => PUnionFind (PrimState m) a -> Int -> Int -> m a
diffPUF !uf !v1 !v2 = (-) <$> potPUF uf v1 <*> potPUF uf v2

-- | \(O(\alpha(N))\) Returns @p(v1) - p(v2)@. Returns @Nothing@ when the two vertices are not
-- connected.
{-# INLINE diffMayPUF #-}
diffMayPUF :: (PrimMonad m, Num a, U.Unbox a) => PUnionFind (PrimState m) a -> Int -> Int -> m (Maybe a)
diffMayPUF !uf !v1 !v2 = do
  b <- samePUF uf v1 v2
  if b
    then Just <$> diffPUF uf v1 v2
    else return Nothing

-- | \(O(1)\)
{-# INLINE clearPUF #-}
clearPUF :: forall m a. (PrimMonad m, Num a, U.Unbox a) => PUnionFind (PrimState m) a -> m ()
clearPUF !uf = do
  UM.set (potencialPUF uf) (0 :: a)
  UM.set (nodesPUF uf) (MUFRoot 1)

-- | \(O(N W)\) Returns vertices by root.
{-# INLINE groupsPUF #-}
groupsPUF :: (PrimMonad m, Num a, U.Unbox a) => PUnionFind (PrimState m) a -> m (IM.IntMap [Int])
groupsPUF uf@(PUnionFind !vec !_) = do
  rvs <- V.generateM (GM.length vec) (\v -> (,[v]) <$> rootPUF uf v)
  return $ IM.fromListWith (flip (++)) $ V.toList rvs
