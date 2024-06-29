{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | A mutable splay tree-based map.
module Data.SplayMap where

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
data SplayNode a = SplayNode
  { lSpNode :: !SplayIndex,
    rSpNode :: !SplayIndex,
    keySpNode :: !Int,
    valSpNode :: !a
  }

-- | Internal representation of `SplayNode a` for implementing `U.Unbox`.
type SplayNodeRepr a = (SplayIndex, SplayIndex, Int, a)

instance U.IsoUnbox (SplayNode a) (SplayNodeRepr a) where
  {-# INLINE toURepr #-}
  toURepr SplayNode {..} = (lSpNode, rSpNode, keySpNode, valSpNode)
  {-# INLINE fromURepr #-}
  fromURepr (!lSpNode, !rSpNode, !keySpNode, !valSpNode) = SplayNode {..}

{- ORMOLU_DISABLE -}
newtype instance U.MVector s (SplayNode a) = MV_SplayNode (UM.MVector s (SplayNodeRepr a))
newtype instance U.Vector (SplayNode a) = V_SplayNode (U.Vector (SplayNodeRepr a))
deriving via (SplayNode a `U.As` SplayNodeRepr a) instance (U.Unbox a) => GM.MVector UM.MVector (SplayNode a)
deriving via (SplayNode a `U.As` SplayNodeRepr a) instance (U.Unbox a) => G.Vector U.Vector (SplayNode a)
instance (U.Unbox a) => U.Unbox (SplayNode a)
{- ORMOLU_ENABLE -}

data SplayMap k v s = SplayMap
  { rootSMap :: !SplayIndex,
    dataSMap :: !(UM.MVector s (SplayNode v))
  }

-- | \(O(N)\) Creates a new `SplayMap` of capacity @n@.
newSMap :: (U.Unbox v, PrimMonad m) => Int -> m (SplayMap k v (PrimState m))
newSMap n = do
  let rootSMap = undefSplayIndex
  dataSMap <- UM.unsafeNew n
  return $ SplayMap {..}

-- | \(O(1)\) Left child rotation.
--
-- = Visualization
--
-- Move up @l@, move @i@ to the right node of @l@, set @lr@ as the left node of @i@.
--
-- @
--      i*           l*       * .. updated
--     / \          / \
--    l*  r  -->  ll   i*
--   / \              / \
-- ll   lr           lr  r
-- @
rotateLSMap :: (U.Unbox v, PrimMonad m) => SplayMap k v (PrimState m) -> SplayIndex -> m ()
rotateLSMap SplayMap {..} i = do
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
--   i*             r*          * .. updated
--  / \            / \
-- l   r*   -->   i*  rr
--    / \        / \
--   rl  rr     l   rl
-- @
rotateRSMap :: (U.Unbox v, PrimMonad m) => SplayMap k v (PrimState m) -> SplayIndex -> m ()
rotateRSMap SplayMap {..} i = do
  -- TODO: check if @v@ is alive
  ni <- GM.read dataSMap i
  let !r = lSpNode ni
  nr <- GM.read dataSMap r
  GM.write dataSMap i $ ni {rSpNode = rSpNode nr}
  GM.write dataSMap r $ ni {lSpNode = r}
  -- TODO: update the original parent of @i@.
  return ()
