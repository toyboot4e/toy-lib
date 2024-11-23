{-# LANGUAGE LambdaCase #-}

-- | Union-find tree under a differencial constraint system on a `Group`
--
-- = Typical problems
-- - [ABC 328 F - Good Set Query](https://atcoder.jp/contests/abc328/tasks/abc328_f)
module Data.UnionFind.Potencial where

import Control.Monad
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Core.Group
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
--
-- = Invariants
-- New comping potencial always comes from left: @new <> old@.
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
newPUF :: forall m a. (PrimMonad m, Monoid a, U.Unbox a) => Int -> m (PUnionFind (PrimState m) a)
newPUF n = PUnionFind <$> UM.replicate n (MUFRoot 1) <*> UM.replicate n (mempty @a)

-- | \(O(\alpha(N))\) Returns root to the given vertex, after updating the internal differencial potencials.
{-# INLINE rootPUF #-}
rootPUF :: (PrimMonad m, Semigroup a, U.Unbox a) => PUnionFind (PrimState m) a -> Int -> m Int
rootPUF uf = inner
  where
    inner v =
      GM.read (nodesPUF uf) v >>= \case
        MUFRoot _ -> pure v
        MUFChild p -> do
          -- NOTE(perf): Path compression.
          -- Handle the nodes closer to the root first and move them onto just under the root
          !r <- inner p
          when (p /= r) $ do
            !pp <- GM.read (potencialPUF uf) p
            -- Move `v` to just under the root:
            GM.write (nodesPUF uf) v (MUFChild r)
            -- Invariant: new coming operators always comes from the left. And we're performing
            -- reverse folding.
            GM.modify (potencialPUF uf) (<> pp) v
          pure r

-- | \(O(\alpha(N))\) Unifies two nodes, managing their differencial potencial so that
-- @p(v1) = dp <> p(v2)@ holds after unification. Or think like @unifyPUF uf dst src@.
--
-- Returns `True` when thry're newly unified. Returns `False` if they're already unified or when
-- the unification didn't match the existing potencials.
{-# INLINE unifyPUF #-}
unifyPUF :: (PrimMonad m, Group a, Ord a, U.Unbox a) => PUnionFind (PrimState m) a -> Int -> Int -> a -> m Bool
unifyPUF !uf !v1 !v2 !dp = do
  !r1 <- rootPUF uf v1
  !r2 <- rootPUF uf v2
  if r1 == r2
    then pure False
    else do
      -- NOTE(perf): Union by size (choose smaller one for root).
      -- Another, more proper optimization would be union by rank (depth).
      !size1 <- GM.read (potencialPUF uf) v1
      !size2 <- GM.read (potencialPUF uf) v2
      if size1 >= size2
        then do
          -- Unify `r1` onto `r2`

          -- Update the size of `r1`
          !sz1 <- _unwrapMUFRoot <$> GM.read (nodesPUF uf) r1
          !sz2 <- _unwrapMUFRoot <$> GM.read (nodesPUF uf) r2
          GM.write (nodesPUF uf) r1 (MUFRoot (sz1 + sz2))

          -- p(v1) becomes p'(v1) under r2 after unified. p(r1) becomes p'(r1).
          --     p'(v1) = dp <> p(v2)
          --     p'(v1) = p(v1) <> 'p(r1)
          -- Therefore,
          --     p'(r1) = p^{-1}(v1) <> dp <> p(v2)
          !p1 <- GM.read (potencialPUF uf) v1
          !p2 <- GM.read (potencialPUF uf) v2
          let !pr1' = invert p1 <> dp <> p2

          -- Move `r1` to just under `r2`:
          GM.write (nodesPUF uf) r1 (MUFChild r2)
          GM.write (potencialPUF uf) r1 pr1'

          pure True
        else do
          unifyPUF uf v2 v1 $ invert dp

-- | \(O(\alpha(N))\) `unifyPUF` with the return value discarded.
{-# INLINE unifyPUF_ #-}
unifyPUF_ :: (PrimMonad m, Group a, Ord a, U.Unbox a) => PUnionFind (PrimState m) a -> Int -> Int -> a -> m ()
unifyPUF_ !uf !v1 !v2 !dp = void $ unifyPUF uf v1 v2 dp

-- More API

-- | \(O(1)\) Returns the number of vertices belonging to the same group.
{-# INLINE sizePUF #-}
sizePUF :: (PrimMonad m, Semigroup a, U.Unbox a) => PUnionFind (PrimState m) a -> Int -> m Int
sizePUF !uf !v = fmap _unwrapMUFRoot . GM.read (nodesPUF uf) =<< rootPUF uf v

-- | \(O(\alpha(N))\) Has the same root / belongs to the same group.
{-# INLINE samePUF #-}
samePUF :: (PrimMonad m, Semigroup a, U.Unbox a) => PUnionFind (PrimState m) a -> Int -> Int -> m Bool
samePUF !uf !v1 !v2 = (==) <$> rootPUF uf v1 <*> rootPUF uf v2

-- | \(O(\alpha(N))\) Can be unified (or already unified) keeping the equiation @p(v1) = dp <> p(v2)@.
{-# INLINE canUnifyPUF #-}
canUnifyPUF :: (PrimMonad m, Semigroup a, Eq a, U.Unbox a) => PUnionFind (PrimState m) a -> Int -> Int -> a -> m Bool
canUnifyPUF !uf !v1 !v2 !dp = do
  !r1 <- rootPUF uf v1
  !r2 <- rootPUF uf v2
  !p1 <- GM.read (potencialPUF uf) v1
  !p2 <- GM.read (potencialPUF uf) v2
  pure $ r1 /= r2 || p1 == dp <> p2

-- | Returns @p(v)@: the potencial of the vertex in their group.
{-# INLINE potPUF #-}
potPUF :: (PrimMonad m, Semigroup a, U.Unbox a) => PUnionFind (PrimState m) a -> Int -> m a
potPUF !uf !v1 = do
  -- Perform path compression
  void $ rootPUF uf v1
  GM.read (potencialPUF uf) v1

-- | \(O(\alpha(N))\) Returns \(p(v1) <> p^{-1}(v2)\). Returns non-meaning value when the two vertices are
-- not connected. Or think like @diffPUF uf dst src@.
{-# INLINE diffPUF #-}
diffPUF :: (PrimMonad m, Group a, U.Unbox a) => PUnionFind (PrimState m) a -> Int -> Int -> m a
diffPUF !uf !v1 !v2 = do
  p1 <- potPUF uf v1
  p2 <- potPUF uf v2
  pure $ p1 <> invert p2

-- | \(O(\alpha(N))\) Returns \(p(v1) <> p^{-1}(v2)\). Returns @Nothing@ when the two vertices are not
-- connected. Or think like @diffMayPUF uf dst src@.
{-# INLINE diffMayPUF #-}
diffMayPUF :: (PrimMonad m, Group a, U.Unbox a) => PUnionFind (PrimState m) a -> Int -> Int -> m (Maybe a)
diffMayPUF !uf !v1 !v2 = do
  b <- samePUF uf v1 v2
  if b
    then Just <$> diffPUF uf v1 v2
    else pure Nothing

-- | \(O(1)\)
{-# INLINE clearPUF #-}
clearPUF :: forall m a. (PrimMonad m, Group a, U.Unbox a) => PUnionFind (PrimState m) a -> m ()
clearPUF !uf = do
  GM.set (potencialPUF uf) (mempty @a)
  GM.set (nodesPUF uf) (MUFRoot 1)

-- | \(O(N W)\) Returns vertices by root.
{-# INLINE groupsPUF #-}
groupsPUF :: (PrimMonad m, Semigroup a, U.Unbox a) => PUnionFind (PrimState m) a -> m (IM.IntMap [Int])
groupsPUF uf@(PUnionFind !vec !_) = do
  rvs <- V.generateM (GM.length vec) (\v -> (,[v]) <$> rootPUF uf v)
  pure $ IM.fromListWith (flip (++)) $ V.toList rvs
