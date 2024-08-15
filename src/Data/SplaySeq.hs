{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | A mutable splay tree-based sequence.
module Data.SplaySeq where

import Control.Exception (assert)
import Control.Monad (unless, void, when)
import Control.Monad.Fix (fix)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Buffer
import Data.Maybe
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Stack (HasCallStack)

-- | Index of a `SplayNode` stored in a `SplayMap`.
type SplayIndex = Int

{-# INLINE undefSI #-}
undefSI :: SplayIndex
undefSI = -1

{-# INLINE nullSI #-}
nullSI :: SplayIndex -> Bool
nullSI = (== undefSI)

-- | Splay tree node.
data SplayNode k v = SplayNode
  { lSN :: {-# UNPACK #-} !SplayIndex,
    rSN :: {-# UNPACK #-} !SplayIndex,
    keySN :: !k,
    valSN :: !v
  }
  deriving (Show, Eq)

-- | Internal representation of `SplayNode a` for implementing `U.Unbox`.
type SplayNodeRepr k v = (SplayIndex, SplayIndex, k, v)

instance U.IsoUnbox (SplayNode k v) (SplayNodeRepr k v) where
  {-# INLINE toURepr #-}
  toURepr SplayNode {..} = (lSN, rSN, keySN, valSN)
  {-# INLINE fromURepr #-}
  fromURepr (!lSN, !rSN, !keySN, !valSN) = SplayNode {..}

{- ORMOLU_DISABLE -}
newtype instance U.MVector s (SplayNode k v) = MV_SplayNode (UM.MVector s (SplayNodeRepr k v))
newtype instance U.Vector (SplayNode k v) = V_SplayNode (U.Vector (SplayNodeRepr k v))
deriving via (SplayNode k v `U.As` SplayNodeRepr k v) instance (U.Unbox k, U.Unbox v) => GM.MVector UM.MVector (SplayNode k v)
deriving via (SplayNode k v `U.As` SplayNodeRepr k v) instance (U.Unbox k, U.Unbox v) => G.Vector U.Vector (SplayNode k v)
instance (U.Unbox k, U.Unbox v) => U.Unbox (SplayNode k v)
{- ORMOLU_ENABLE -}

-- | Mutable, splay tree-based map.
data SplaySeq k v s = SplaySeq
  { -- | The maximum number of elements.
    capacitySS :: {-# UNPACK #-} !Int,
    -- | Index of the root node.
    rootSS :: !(UM.MVector s SplayIndex),
    -- | Data storage.
    dataSS :: !(Buffer s (SplayNode k v))
  }

-- | \(O(N)\) Creates a new `SplaySeq` of capacity @n@.
{-# INLINE newSS #-}
newSS :: (U.Unbox k, U.Unbox v, PrimMonad m) => Int -> m (SplaySeq k v (PrimState m))
newSS n = do
  rootSS <- UM.replicate 1 undefSI
  dataSS <- newBuffer n
  return $ SplaySeq {capacitySS = n, ..}

-- | \(O(N)\) Creates a new `SplaySeq` of capacity @n@ with initial values @xs@.
--
-- TODO: faster implementation?
{-# INLINE buildSS #-}
buildSS :: (Ord k, U.Unbox k, U.Unbox v, PrimMonad m) => Int -> U.Vector (k, v) -> m (SplaySeq k v (PrimState m))
buildSS n xs = do
  sm <- newSS n
  U.forM_ xs $ \(!k, !v) -> do
    insertSS sm k v
  return sm

-- | \(O(1)\) Resets the splay tree to the initial state.
{-# INLINE clearSS #-}
clearSS :: (PrimMonad m) => SplaySeq k v (PrimState m) -> m ()
clearSS SplaySeq {..} = do
  clearBuffer dataSS

{-# INLINE lengthSS #-}
lengthSS :: (PrimMonad m) => SplaySeq k v (PrimState m) -> m Int
lengthSS = lengthBuffer . dataSS

-- * Internal update

-- | Efficiently modify `SplayNode`.
{-# INLINE writeLSS #-}
writeLSS :: (HasCallStack, PrimMonad m) => Buffer (PrimState m) (SplayNode k v) -> SplayIndex -> SplayIndex -> m ()
writeLSS (internalBuffer -> MV_SplayNode (U.MV_4 _ l _ _ _)) i l' = do
  GM.write l i l'

-- | Efficiently modify `SplayNode`.
{-# INLINE writeRSS #-}
writeRSS :: (HasCallStack, PrimMonad m) => Buffer (PrimState m) (SplayNode k v) -> SplayIndex -> SplayIndex -> m ()
writeRSS (internalBuffer -> MV_SplayNode (U.MV_4 _ _ r _ _)) i r' = do
  GM.write r i r'

-- | Efficiently modify `SplayNode`.
{-# INLINE writeKSS #-}
writeKSS :: (HasCallStack, U.Unbox k, PrimMonad m) => Buffer (PrimState m) (SplayNode k v) -> SplayIndex -> k -> m ()
writeKSS (internalBuffer -> MV_SplayNode (U.MV_4 _ _ _ k _)) i k' = do
  GM.write k i k'

-- | Efficiently modify `SplayNode`.
{-# INLINE writeVSS #-}
writeVSS :: (HasCallStack, U.Unbox v, PrimMonad m) => Buffer (PrimState m) (SplayNode k v) -> SplayIndex -> v -> m ()
writeVSS (internalBuffer -> MV_SplayNode (U.MV_4 _ _ _ _ v)) i v' = do
  GM.write v i v'

-- | Efficiently read `SplayNode`.
{-# INLINE readLSS #-}
readLSS :: (HasCallStack, PrimMonad m) => Buffer (PrimState m) (SplayNode k v) -> SplayIndex -> m SplayIndex
readLSS (internalBuffer -> MV_SplayNode (U.MV_4 _ l _ _ _)) i = do
  GM.read l i

-- | Efficiently read `SplayNode`.
{-# INLINE readRSS #-}
readRSS :: (HasCallStack, PrimMonad m) => Buffer (PrimState m) (SplayNode k v) -> SplayIndex -> m SplayIndex
readRSS (internalBuffer -> MV_SplayNode (U.MV_4 _ _ r _ _)) i = do
  GM.read r i

-- | Efficiently read `SplayNode`.
{-# INLINE readKSS #-}
readKSS :: (HasCallStack, U.Unbox k, PrimMonad m) => Buffer (PrimState m) (SplayNode k v) -> SplayIndex -> m k
readKSS (internalBuffer -> MV_SplayNode (U.MV_4 _ _ _ k _)) i = do
  GM.read k i

-- | Efficiently read `SplayNode`.
{-# INLINE readVSS #-}
readVSS :: (HasCallStack, U.Unbox v, PrimMonad m) => Buffer (PrimState m) (SplayNode k v) -> SplayIndex -> m v
readVSS (internalBuffer -> MV_SplayNode (U.MV_4 _ _ _ _ v)) i = do
  GM.read v i
i
