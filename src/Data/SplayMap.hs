{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | A mutable splay tree-based map.
--
-- = Invariants
--
-- Splay tree is a self-adjusting tree that follows the splaying operation heuristic.
--
-- <http://www.cs.cmu.edu/~sleator/papers/self-adjusting.pdf>
--
-- = Invariants
--
-- Left children have keys that are less than or equal to their parent's one. Right children have
-- bigger keys than their parent.
--
-- = Thanks
-- - [Splay Tree: One Tree to Rule Them All](https://zhtluo.com/cp/splay-tree-one-tree-to-rule-them-all.html)
-- - [sile/splay_tree](https://github.com/sile/splay_tree)
module Data.SplayMap where

import Control.Exception (assert)
import Control.Monad (unless, when)
import Control.Monad.Fix (fix)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Buffer
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Stack (HasCallStack)

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
    -- | Index of the root node.
    rootSMap :: !(UM.MVector s SplayIndex),
    -- | Data storage.
    dataSMap :: !(Buffer s (SplayNode k v))
  }

-- | \(O(N)\) Creates a new `SplayMap` of capacity @n@.
newSMap :: (U.Unbox k, U.Unbox v, PrimMonad m) => Int -> m (SplayMap k v (PrimState m))
newSMap n = do
  rootSMap <- UM.replicate 1 undefSplayIndex
  dataSMap <- newBuffer n
  return $ SplayMap {capacitySMap = n, ..}

-- | \(O(N)\) Creates a new `SplayMap` of capacity @n@ with initial values @xs@.
--
-- TODO: faster implementation?
buildSMap :: (Ord k, U.Unbox k, U.Unbox v, PrimMonad m) => Int -> U.Vector (k, v) -> m (SplayMap k v (PrimState m))
buildSMap n xs = do
  smap <- newSMap n
  U.forM_ xs $ \(!k, !v) -> do
    insertSMap smap k v
  return smap

lengthSMap :: (U.Unbox k, U.Unbox v, PrimMonad m) => SplayMap k v (PrimState m) -> m Int
lengthSMap = lengthBuffer . dataSMap

-- * Splay

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
_rotateLSMap :: (HasCallStack, U.Unbox k, U.Unbox v, PrimMonad m) => SplayMap k v (PrimState m) -> SplayIndex -> m ()
_rotateLSMap SplayMap {..} i = do
  -- TODO: check if @v@ is alive
  ni <- readFront dataSMap i
  let !l = lSpNode ni
  nl <- readFront dataSMap l
  writeFront dataSMap i $ ni {lSpNode = lSpNode nl}
  writeFront dataSMap l $ ni {rSpNode = i}
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
_rotateRSMap :: (HasCallStack, U.Unbox k, U.Unbox v, PrimMonad m) => SplayMap k v (PrimState m) -> SplayIndex -> m ()
_rotateRSMap SplayMap {..} i = do
  -- TODO: check if @v@ is alive
  ni <- readFront dataSMap i
  let !r = lSpNode ni
  nr <- readFront dataSMap r
  writeFront dataSMap i $ ni {rSpNode = rSpNode nr}
  writeFront dataSMap r $ ni {lSpNode = r}
  -- TODO: update the original parent of @i@.
  return ()

-- | Amortized \(O(\log N)\) Splay @v@ so that it is under @r@ (or to the root if s is null).
--
-- = Top-down splaying
--
-- The are two known approaches for the splaying operation: bottom-up and top-down. The former is
-- easier to understand but less efficient. The latter is faster and uses less memory.
--
-- See also: <http://www.cs.cmu.edu/~sleator/papers/self-adjusting.pdf#16>
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
splaySMap :: (HasCallStack, Ord k, U.Unbox k, U.Unbox v, PrimMonad m) => SplayMap k v (PrimState m) -> SplayIndex -> k -> m (SplayIndex, Ordering)
splaySMap smap@SplayMap {..} i0 key = inner i0 undefSplayIndex undefSplayIndex
  where
    -- @inner@ goes down the tree to find the target @key@ while performing the splaying operation.
    -- @inner@ manages the folloing three variables:
    -- - middle tree: a tree rooted by the current node.
    -- - left tree: a tree with keys less than or equal to the current node's key.
    -- - right tree: a tree with keys bigger than the current node's key.
    inner iM iL iR = do
      nodeM <- readFront dataSMap iM
      case compare key (keySpNode nodeM) of
        LT | not (nullSplayIndex (lSpNode nodeM)) -> do
          nodeML <- readFront dataSMap (lSpNode nodeM)
          -- (ii) zig-zig: same direction twice, rotate right the child.
          when (key < keySpNode nodeML && not (nullSplayIndex (lSpNode nodeML))) $ do
            _rotateRSMap smap (lSpNode nodeM)
          -- link left:
          unless (nullSplayIndex iL) $ do
            writeRChild iL iM
          inner (rSpNode nodeM) iM iR
        GT | not (nullSplayIndex (rSpNode nodeM)) -> do
          nodeMR <- readFront dataSMap (rSpNode nodeM)
          -- (ii) zig-zig: same direction twice, rotate the child.
          when (key > keySpNode nodeMR && not (nullSplayIndex (rSpNode nodeMR))) $ do
            _rotateLSMap smap (rSpNode nodeM)
          -- link right:
          unless (nullSplayIndex iR) $ do
            writeLChild iR iM
          inner (lSpNode nodeM) iL iM
        _ -> do
          done iM nodeM iL iR
          return . (iM,) $! compare key (keySpNode nodeM)

    done iM nodeM iL iR = do
      writeLChild iR (rSpNode nodeM)
      writeRChild iL (rSpNode nodeM)
      -- FIXME: what??
      -- writeLChild iU undefSplayIndex
      -- writeRChild iU undefSplayIndex
      writeLChild iM iL
      writeRChild iM iR
    writeLChild iParent iChild = do
      -- TODO: more efficient update?
      parent <- readFront dataSMap iParent
      writeFront dataSMap iParent $ parent {lSpNode = iChild}
    writeRChild iParent iChild = do
      -- TODO: more efficient update?
      parent <- readFront dataSMap iParent
      writeFront dataSMap iParent $ parent {rSpNode = iChild}

-- splayFromRootSMap :: (HasCallStack, Ord k, U.Unbox k, U.Unbox v, PrimMonad m) => SplayMap k v (PrimState m) -> k -> m SplayIndex
-- splayFromRootSMap smap@SplayMap {..} k = do
--   root <- UM.read rootSMap i
--   if nullSplayIndex root
--     then return undefSplayIndex
--     else splaySMap smap root k

-- walkSMap :: (HasCallStack, Ord k, U.Unbox k, U.Unbox v, PrimMonad m) => SplayMap k v (PrimState m) -> SplayIndex -> m (Maybe v)
-- walkSMap SplayMap {..} i = do
--   ni <- readFront dataSMap i
--   return Nothing

-- | Amortized \(O(\log N)\).
pushRootSMap :: (HasCallStack, Ord k, U.Unbox k, U.Unbox v, PrimMonad m) => SplayMap k v (PrimState m) -> k -> v -> m ()
pushRootSMap SplayMap {..} k v = do
  pushBack dataSMap $ SplayNode undefSplayIndex undefSplayIndex k v
  len <- lengthBuffer dataSMap
  GM.write rootSMap 0 (len - 1)

-- * API

-- | Amortized \(O(\log N)\).
lookupSMap :: (HasCallStack, Ord k, U.Unbox k, U.Unbox v, PrimMonad m) => SplayMap k v (PrimState m) -> k -> m (Maybe v)
lookupSMap smap k = do
  return Nothing

-- | Amortized \(O(\log N)\).
insertSMap :: (HasCallStack, Ord k, U.Unbox k, U.Unbox v, PrimMonad m) => SplayMap k v (PrimState m) -> k -> v -> m (Maybe v)
insertSMap smap@SplayMap {..} k v = do
  root <- GM.read rootSMap 0
  if nullSplayIndex root
    then do
      pushRootSMap smap k v
      return Nothing
    else do
      (!root', !ordering) <- splaySMap smap root k
      UM.write rootSMap 0 root'
      case ordering of
        EQ -> do
          return Nothing
        LT -> do
          return Nothing
        GT -> do
          return Nothing

-- -- | \(O(1)\)
-- _swapRemoveSMap :: (U.Unbox k, U.Unbox v, PrimMonad m) => SplayMap k v (PrimState m) -> Int -> m (Maybe v)
-- _swapRemoveSMap SplayMap {..} i = do
--   let !_ = assert (i >= 0 && i < n) $ "invalid index for removal: " ++ show (0, n - 1) ++ " " ++ show i
--   GM.swap dataSMap i (n - 1)
--   return Nothing

-- | Returns @(key, value)@ pairs sorted by the keys in ascending order.
dfsSMap :: (HasCallStack, U.Unbox k, U.Unbox v, PrimMonad m) => SplayMap k v (PrimState m) -> m (U.Vector (k, v))
dfsSMap smap@SplayMap {..} = do
  buf <- newBuffer =<< lengthSMap smap
  root <- GM.read rootSMap 0
  flip fix root $ \loop i -> do
    unless (nullSplayIndex i) $ do
      node <- readFront dataSMap i
      unless (nullSplayIndex (lSpNode node)) $ do
        loop $ lSpNode node
      pushBack buf (keySpNode node, valSpNode node)
      unless (nullSplayIndex (rSpNode node)) $ do
        loop $ rSpNode node
  unsafeFreezeBuffer buf

