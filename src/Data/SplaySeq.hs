{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
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

-- TODO: use abstract type

-- | Splay tree node.
data SplayNode v = SplayNode
  { -- -- | Parent node.
    -- pSN :: {-# UNPACK #-} !SplayIndex,
    -- -- | Left child node.
    -- lSN :: {-# UNPACK #-} !SplayIndex,
    -- -- | Right child node.
    -- rSN :: {-# UNPACK #-} !SplayIndex,

    -- | The payload.
    valSN :: !v
  }
  deriving (Show, Eq)

-- | Internal representation of `SplayNode a` for implementing `U.Unbox`.
type SplayNodeRepr v = v

instance U.IsoUnbox (SplayNode v) (SplayNodeRepr v) where
  {-# INLINE toURepr #-}
  toURepr SplayNode {..} = valSN
  {-# INLINE fromURepr #-}
  fromURepr !valSN = SplayNode {..}

{- ORMOLU_DISABLE -}
newtype instance U.MVector s (SplayNode v) = MV_SplayNode (UM.MVector s (SplayNodeRepr v))
newtype instance U.Vector (SplayNode v) = V_SplayNode (U.Vector (SplayNodeRepr v))
deriving via (SplayNode v `U.As` SplayNodeRepr v) instance (U.Unbox v) => GM.MVector UM.MVector (SplayNode v)
deriving via (SplayNode v `U.As` SplayNodeRepr v) instance (U.Unbox v) => G.Vector U.Vector (SplayNode v)
instance (U.Unbox v) => U.Unbox (SplayNode v)
{- ORMOLU_ENABLE -}

-- | Mutable, splay tree-based map.
data SplaySeq s v = SplaySeq
  { -- | The maximum number of elements.
    capacitySS :: {-# UNPACK #-} !Int,
    -- | Index of the root node.
    rootSS :: !(UM.MVector s SplayIndex),
    -- | Free slots in the data storage for \(O(1)\) node allocation.
    freeSS :: !(Buffer s Int),
    -- | Decomposed node data storage: left children.
    lSS :: !(UM.MVector s SplayIndex),
    -- | Decomposed node data storage: right children.
    rSS :: !(UM.MVector s SplayIndex),
    -- | Decomposed node data storage: left parents
    pSS :: !(UM.MVector s SplayIndex),
    -- | Decomposed node data storage: payloads.
    vSS :: !(UM.MVector s v)
  }

-- | \(O(N)\) Creates a new `SplaySeq` of capacity @n@.
{-# INLINE newSS #-}
newSS :: (U.Unbox v, PrimMonad m) => Int -> m (SplaySeq (PrimState m) v)
newSS n = do
  rootSS <- UM.replicate 1 undefSI
  freeSS <- generateBuffer n id
  lSS <- UM.unsafeNew n
  rSS <- UM.unsafeNew n
  pSS <- UM.unsafeNew n
  vSS <- UM.unsafeNew n
  return $ SplaySeq {capacitySS = n, ..}

-- TODO: newBuffer

-- | \(O(1)\) Resets the splay tree to the initial state.
{-# INLINE clearSS #-}
clearSS :: (PrimMonad m) => SplaySeq (PrimState m) v -> m ()
clearSS SplaySeq {..} = do
  clearBuffer freeSS

-- {-# INLINE lengthSS #-}
-- lengthSS :: (PrimMonad m) => SplaySeq (PrimState m) v -> m Int
-- lengthSS = UM.length . dataSS

-- | \(O(1)\) Allocates a new node.
allocNodeSS :: (HasCallStack, PrimMonad m, U.Unbox v) => SplaySeq (PrimState m) v -> v -> m SplayIndex
allocNodeSS seq@SplaySeq {..} !x = do
  i <- fromJust <$> popFront freeSS
  -- FIXME:
  GM.write vSS i x
  return i

-- | \(O(1)\) Frees a node.
freeNodeSS :: (PrimMonad m) => SplaySeq (PrimState m) v -> SplayIndex -> m ()
freeNodeSS seq@SplaySeq {..} !i = do
  pushBack freeSS i

-- | \(O(N)\) Frees a subtree.
freeSubtreeSS :: (HasCallStack, PrimMonad m, U.Unbox v) => SplaySeq (PrimState m) v -> SplayIndex -> m ()
freeSubtreeSS seq@SplaySeq {..} =
  fix $ \dfs i -> do
    -- FIXME: efficiently read part of the node
    -- TODO: optics?
    l <- GM.read lSS i
    r <- GM.read rSS i
    -- free children first
    unless (nullSI l) $ dfs l
    unless (nullSI r) $ dfs r
    freeNodeSS seq i

-- | \(O(1)\) Rotates a node. Propagation and updates are done outside of the function.
rotateSS :: (PrimMonad m, U.Unbox v) => SplaySeq (PrimState m) v -> SplayIndex -> m ()
rotateSS seq@SplaySeq {..} !i = do
  p <- GM.read pSS i
  l <- GM.read lSS i
  r <- GM.read rSS i

  pp <- GM.read pSS p
  pl <- GM.read lSS p
  pr <- GM.read rSS p

  -- TODO: check what's going on
  c <-
    if pl == i
      then do
        GM.write rSS i p
        GM.write lSS p r
        return r
      else do
        GM.write lSS i p
        GM.write rSS p l
        return l

  unless (nullSI pp) $ do
    ppl <- GM.read lSS pp
    ppr <- GM.read rSS pp
    when (ppl == p) $ do
      GM.write lSS pp i
    when (ppr == p) $ do
      GM.write rSS pp i

  GM.write pSS i pp
  GM.write pSS p i

  unless (nullSI c) $ do
    GM.write pSS c p

