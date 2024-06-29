{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | A splay tree implementation.
module Data.SplayMap where

import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

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

newtype instance U.MVector s (SplayNode a) = MV_SplayNode (UM.MVector s (SplayNodeRepr a))

newtype instance U.Vector (SplayNode a) = V_SplayNode (U.Vector (SplayNodeRepr a))

deriving via (SplayNode a `U.As` SplayNodeRepr a) instance (U.Unbox a) => GM.MVector UM.MVector (SplayNode a)

deriving via (SplayNode a `U.As` SplayNodeRepr a) instance (U.Unbox a) => G.Vector U.Vector (SplayNode a)

instance (U.Unbox a) => U.Unbox (SplayNode a)

data SplayMap k v s = SplayMap
  { rootSpT :: !SplayIndex,
    dataSpT :: !(UM.MVector s v)
  }

newSM :: (U.Unbox v, PrimMonad m) => Int -> m (SplayMap k v (PrimState m))
newSM n = do
  let rootSpT = undefSplayIndex
  dataSpT <- UM.unsafeNew n
  return $ SplayMap {..}

