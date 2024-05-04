{-# LANGUAGE LambdaCase #-}

-- | Union-find tree under a differencial constraint system.
--
-- = Typical problems
-- - [ABC 328 F - Good Set Query](https://atcoder.jp/contests/abc328/tasks/abc328_f)
module Data.UnionFind.Potencial where

import Control.Monad
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.UnionFind.Mutable (MUFNode (..), _unwrapMUFRoot)
import qualified Data.Vector.Unboxed.Mutable as UM

-- | Union-Find tree under a differencial constraint systems. Each vertex @v@ is given potencial
-- value @p(v)@ which is a relative value tested on `unifyPUF`.
--
-- Implementation based on: <https://qiita.com/drken/items/cce6fc5c579051e64fab>
data PUnionFind s = PUnionFind
  { -- | Node data (@MUFParent size | MUFNode parent@).
    nodesPUF :: UM.MVector s MUFNode,
    -- | Diffierencial potencial of each vertex.
    potencialPUF :: UM.MVector s Int
  }

-- TODO: consider using `unsafe` when the performance is important

-- Fundamentals

-- | Creates a new union-find tree under a differencial-potencal system.
newPUF :: (PrimMonad m) => Int -> m (PUnionFind (PrimState m))
newPUF n = PUnionFind <$> UM.replicate n (MUFRoot 1) <*> UM.replicate n (0 :: Int)

-- | Returns root to the given vertex, after updating the internal differencial potencials.
rootPUF :: (PrimMonad m) => PUnionFind (PrimState m) -> Int -> m Int
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

-- | Unifies two nodes, managing their differencial potencial so that @p(v1) - p(v2) = d@ holds.
-- Returns `True` when thry're newly unified. Returns `False` if they're already unified or when the
-- unification didn't match the existing potencials.
unifyPUF :: (PrimMonad m) => PUnionFind (PrimState m) -> Int -> Int -> Int -> m Bool
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

-- More API

-- | Returns the number of vertices belonging to the same group.
sizePUF :: (PrimMonad m) => PUnionFind (PrimState m) -> Int -> m Int
sizePUF !uf !v = fmap _unwrapMUFRoot . UM.read (nodesPUF uf) =<< rootPUF uf v

-- | Has the same root / belongs to the same group.
samePUF :: (PrimMonad m) => PUnionFind (PrimState m) -> Int -> Int -> m Bool
samePUF !uf !v1 !v2 = (==) <$> rootPUF uf v1 <*> rootPUF uf v2

-- | Can be unified, keeping the equiation @p(v1) - p(v2) = d@.
canUnifyPUF :: (PrimMonad m) => PUnionFind (PrimState m) -> Int -> Int -> Int -> m Bool
canUnifyPUF !uf !v1 !v2 !d = do
  !r1 <- rootPUF uf v1
  !r2 <- rootPUF uf v2
  !p1 <- UM.read (potencialPUF uf) v1
  !p2 <- UM.read (potencialPUF uf) v2
  return $ r1 /= r2 || p1 - p2 == d

-- | Returns @p(v)@: the potencial of the vertex in their group.
potPUF :: (PrimMonad m) => PUnionFind (PrimState m) -> Int -> m Int
potPUF !uf !v1 = do
  -- Perform path compression
  void $ rootPUF uf v1
  UM.read (potencialPUF uf) v1

-- | Returns @p(v1) - p(v2)@.
diffPUF :: (PrimMonad m) => PUnionFind (PrimState m) -> Int -> Int -> m Int
diffPUF !uf !v1 !v2 = (-) <$> potPUF uf v1 <*> potPUF uf v2

clearPUF :: (PrimMonad m) => PUnionFind (PrimState m) -> m ()
clearPUF !uf = do
  UM.set (potencialPUF uf) (0 :: Int)
  UM.set (nodesPUF uf) (MUFRoot 1)

groupsPUF :: (HasCallStack, PrimMonad m) => PUnionFind (PrimState m) -> m (IM.IntMap [Int])
groupsPUF uf@(PUnionFind !vec !_) = do
  rvs <- V.generateM (GM.length vec) (\v -> (,[v]) <$> rootPUF uf v)
  return $ IM.fromListWith (flip (++)) $ V.toList rvs

