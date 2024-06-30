{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | A mutable splay tree-based map.
--
-- = Thanks
-- - [Splay Tree: One Tree to Rule Them All](https://zhtluo.com/cp/splay-tree-one-tree-to-rule-them-all.html)
-- - [sile/splay_tree](https://github.com/sile/splay_tree)
module Data.SplayMap where

import Control.Exception (assert)
import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- | Index of a `SplayNode` stored in a `SplayMap`.
type SplayIndex = Int

undefSplayIndex :: SplayIndex
undefSplayIndex = -1

nullSplayIndex :: SplayIndex -> Bool
nullSplayIndex = (== undefSplayIndex)

-- | Splay tree node.
data SplayNode k v = SplayNode
  { lSpNode :: !SplayIndex,
    rSpNode :: !SplayIndex,
    keySpNode :: !k,
    valSpNode :: !v
  }

-- | Internal representation of `SplayNode a` for implementing `U.Unbox`.
type SplayNodeRepr k v = (SplayIndex, SplayIndex, k, v)

instance U.IsoUnbox (SplayNode k v) (SplayNodeRepr k v) where
  {-# INLINE toURepr #-}
  toURepr SplayNode {..} = (lSpNode, rSpNode, keySpNode, valSpNode)
  {-# INLINE fromURepr #-}
  fromURepr (!lSpNode, !rSpNode, !keySpNode, !valSpNode) = SplayNode {..}

{- ORMOLU_DISABLE -}
newtype instance U.MVector s (SplayNode k v) = MV_SplayNode (UM.MVector s (SplayNodeRepr k v))
newtype instance U.Vector (SplayNode k v) = V_SplayNode (U.Vector (SplayNodeRepr k v))
deriving via (SplayNode k v `U.As` SplayNodeRepr k v) instance (U.Unbox k, U.Unbox v) => GM.MVector UM.MVector (SplayNode k v)
deriving via (SplayNode k v `U.As` SplayNodeRepr k v) instance (U.Unbox k, U.Unbox v) => G.Vector U.Vector (SplayNode k v)
instance (U.Unbox k, U.Unbox v) => U.Unbox (SplayNode k v)
{- ORMOLU_ENABLE -}

-- | Mutable, splay tree-based map.
data SplayMap k v s = SplayMap
  { -- | The maximum number of elements.
    capacitySMap :: !Int,
    -- | The number of elements stored in `SplayMap`.
    sizeSMap :: !(UM.MVector s Int),
    -- | Index of the root node.
    rootSMap :: !SplayIndex,
    -- | Data storage.
    dataSMap :: !(UM.MVector s (SplayNode k v))
  }

-- | \(O(N)\) Creates a new `SplayMap` of capacity @n@.
newSMap :: (U.Unbox k, U.Unbox v, PrimMonad m) => Int -> m (SplayMap k v (PrimState m))
newSMap n = do
  let rootSMap = undefSplayIndex
  sizeSMap <- UM.replicate 0 (0 :: Int)
  dataSMap <- UM.unsafeNew n
  return $ SplayMap {capacitySMap = n, ..}

-- | \(O(1)\) Left child rotation.
--
-- = Visualization
--
-- Move up @l@, move @i@ to the right node of @l@, set @lr@ as the left node of @i@.
--
-- @
--      i*           l        * .. the side of the child is updated
--     / \          / \
--    l*  r  -->  ll   i
--   / \              / \
-- ll   lr           lr  r
-- @
_rotateLSMap :: (U.Unbox k, U.Unbox v, PrimMonad m) => SplayMap k v (PrimState m) -> SplayIndex -> m ()
_rotateLSMap SplayMap {..} i = do
  -- TODO: check if @v@ is alive
  ni <- GM.read dataSMap i
  let !l = lSpNode ni
  nl <- GM.read dataSMap l
  GM.write dataSMap i $ ni {lSpNode = lSpNode nl}
  GM.write dataSMap l $ ni {rSpNode = i}
  -- TODO: update the original parent of @i@.
  return ()

-- | \(O(1)\) Right child rotation.
--
-- = Visualization
--
-- Move up @r@, move @i@ to the left node of @r@, set @rl@ as the right node of @i@.
--
-- @
--   i*             r           * .. the side of the child is updated
--  / \            / \
-- l   r*   -->   i   rr
--    / \        / \
--   rl  rr     l   rl
-- @
_rotateRSMap :: (U.Unbox k, U.Unbox v, PrimMonad m) => SplayMap k v (PrimState m) -> SplayIndex -> m ()
_rotateRSMap SplayMap {..} i = do
  -- TODO: check if @v@ is alive
  ni <- GM.read dataSMap i
  let !r = lSpNode ni
  nr <- GM.read dataSMap r
  GM.write dataSMap i $ ni {rSpNode = rSpNode nr}
  GM.write dataSMap r $ ni {lSpNode = r}
  -- TODO: update the original parent of @i@.
  return ()

-- | Amortized \(O(\log N)\) Splay @v@ so that it is under @r@ (or to the root if s is null).
--
-- = Visualization
--
-- == Rotate twice
--
-- Example: rotate @lr@ twice:
--
-- @
--      i               *i                lr
--     / \              / \           /        \
--    l*  r   -->     lr*  r  -->    l          i
--   / \             /  \           / \        / \
-- ll  *lr          l    lrr      ll   lrl   lrr  r
--     /  \        / \
--  lrl    lrr   ll   lrl
-- @
--
-- Example: rotate @rl@ twice:
--
-- @
--      i                i                lr
--     / \              / \           /        \
--    l   r   -->     lr   r  -->    l          i
--       / \         /  \           / \        / \
--     rl   rr      l    lrr      ll   lrl   lrr  r
--    /  \         / \
-- rll    rl     ll   lrl
-- @
--
-- == Zig
--
-- For the node @ll@, if it an and the parent @l@ are left children, we rotate @l@ first, and the @ll@:
--
-- @
--         *i              *l                  ll
--         / \          /      \              /  \
--        l*  r  -->  ll*       i    -->   lll    l
--       / \          / \      / \               / \
--     ll   lr     lll   llr lr   r           llr   i
--     / \                                         / \
--  lll   llr                                    lr   r
-- @
--
-- For the node @rr@, if it and the parent @r@ are right children, we rotate @l@ first, and the @ll@:
--
-- @
--   i*                     r*                       rr
--  / \                 /       \                  /    \
-- l  *r        -->    i        *rr      -->      r      llr
--    / \             / \       /  \             / \
--  rl   rr          l   rl  lll    llr         i   lll
--      / \                                    / \
--   rrl   rrr                                l   rl
-- @
--
-- == Zag
--
-- If the parent is the root, then what?
splaySMap :: (U.Unbox k, U.Unbox v, PrimMonad m) => SplayMap k v (PrimState m) -> SplayIndex -> SplayIndex -> m ()
splaySMap SplayMap {..} v r = do
  return ()

-- | Amortized \(O(\log zn)\).
lookupSMap :: (Ord k, U.Unbox k, U.Unbox v, PrimMonad m) => SplayMap k v (PrimState m) -> k -> m (Maybe v)
lookupSMap smap k = do
  return Nothing

-- | Amortized \(O(\log zn)\).
insertSMap :: (Ord k, U.Unbox k, U.Unbox v, PrimMonad m) => SplayMap k v (PrimState m) -> k -> v -> m (Maybe v)
insertSMap smap k v = do
  return Nothing

-- | \(O(1)\)
_swapRemoveSMap :: (U.Unbox k, U.Unbox v, PrimMonad m) => SplayMap k v (PrimState m) -> Int -> m (Maybe v)
_swapRemoveSMap SplayMap {..} i = do
  n <- GM.read sizeSMap 0
  let !_ = assert (i >= 0 && i < n) $ "invalid index for removal: " ++ show (0, n - 1) ++ " " ++ show i
  GM.swap dataSMap i (n - 1)
  UM.modify sizeSMap (subtract 1) 0
  return Nothing
