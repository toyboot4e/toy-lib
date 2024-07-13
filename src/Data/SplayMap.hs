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
-- Left children have keys smaller keys than the parent. Right children have bigger keys than the
-- parent.
--
-- = Thanks
-- - [Splay Tree: One Tree to Rule Them All](https://zhtluo.com/cp/splay-tree-one-tree-to-rule-them-all.html)
-- - [sile/splay_tree](https://github.com/sile/splay_tree)
module Data.SplayMap where

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
  deriving (Show, Eq)

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
    capacitySM :: !Int,
    -- | Index of the root node.
    rootSM :: !(UM.MVector s SplayIndex),
    -- | Data storage.
    dataSM :: !(Buffer s (SplayNode k v))
  }

-- | \(O(N)\) Creates a new `SplayMap` of capacity @n@.
newSM :: (U.Unbox k, U.Unbox v, PrimMonad m) => Int -> m (SplayMap k v (PrimState m))
newSM n = do
  rootSM <- UM.replicate 1 undefSplayIndex
  dataSM <- newBuffer n
  return $ SplayMap {capacitySM = n, ..}

-- | \(O(N)\) Creates a new `SplayMap` of capacity @n@ with initial values @xs@.
--
-- TODO: faster implementation?
buildSM :: (Ord k, U.Unbox k, U.Unbox v, PrimMonad m) => Int -> U.Vector (k, v) -> m (SplayMap k v (PrimState m))
buildSM n xs = do
  sm <- newSM n
  U.forM_ xs $ \(!k, !v) -> do
    insertSM sm k v
  return sm

lengthSM :: (PrimMonad m) => SplayMap k v (PrimState m) -> m Int
lengthSM = lengthBuffer . dataSM

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
--
-- Returns @l@.
rotateRSM :: (HasCallStack, U.Unbox k, U.Unbox v, PrimMonad m) => SplayMap k v (PrimState m) -> SplayIndex -> m SplayIndex
rotateRSM SplayMap {..} i = do
  nodeI <- readFront dataSM i
  let !il = lSpNode nodeI
  nodeIL <- readFront dataSM il
  writeFront dataSM i $ nodeI {lSpNode = rSpNode nodeIL}
  writeFront dataSM il $ nodeIL {rSpNode = i}
  return il

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
--
-- Returns @r@.
rotateLSM :: (HasCallStack, U.Unbox k, U.Unbox v, PrimMonad m) => SplayMap k v (PrimState m) -> SplayIndex -> m SplayIndex
rotateLSM SplayMap {..} i = do
  nodeI <- readFront dataSM i
  let !ir = rSpNode nodeI
  nodeIR <- readFront dataSM ir
  writeFront dataSM i $ nodeI {rSpNode = lSpNode nodeIR}
  writeFront dataSM ir $ nodeIR {lSpNode = i}
  return ir

-- | Amortized \(O(\log N)\) Splay @v@ so that it is under @r@ (or to the root if s is null).
--
-- = Top-down splaying
--
-- The are two known approaches for the splaying operation: bottom-up and top-down. The former is
-- easier to understand but less efficient. The latter is faster and uses less memory.
--
-- See also: <http://www.cs.cmu.edu/~sleator/papers/self-adjusting.pdf#16>
splayBySM :: (HasCallStack, U.Unbox k, U.Unbox v, PrimMonad m) => SplayMap k v (PrimState m) -> (k -> Ordering) -> SplayIndex -> m (SplayIndex, Ordering)
splayBySM sm@SplayMap {..} !cmpF !i0 = do
  lrs <- UM.replicate 2 undefSplayIndex

  -- @inner@ goes down the tree to find the target @key@ while performing the splaying operation.
  -- @inner@ manages the folloing three variables:
  -- - middle tree: a tree rooted by the current node.
  -- - left tree: a tree with keys less than or equal to the current node's key.
  -- - right tree: a tree with keys bigger than the current node's key.
  let inner iM iL iR = do
        when (iM == iL || iM == iR) $ error "wrong"
        nodeM <- readFront dataSM iM
        case cmpF (keySpNode nodeM) of
          LT | not (nullSplayIndex (lSpNode nodeM)) -> do
            iM' <- do
              nodeML <- readFront dataSM (lSpNode nodeM)
              if not (nullSplayIndex (lSpNode nodeML)) && cmpF (keySpNode nodeML) == LT
                then rotateRSM sm iM
                else return iM
            -- link right:
            if nullSplayIndex iR
              then do
                GM.write lrs 1 iM'
              else writeLChild iR iM'
            iM'' <- lSpNode <$> readFront dataSM iM'
            inner iM'' iL iM'
          GT | not (nullSplayIndex (rSpNode nodeM)) -> do
            iM' <- do
              nodeMR <- readFront dataSM (rSpNode nodeM)
              if not (nullSplayIndex (rSpNode nodeMR)) && cmpF (keySpNode nodeMR) == GT
                then rotateLSM sm iM
                else return iM
            -- link left:
            if nullSplayIndex iL
              then do
                GM.write lrs 0 iM'
              else do
                -- FIXME: right child of iM' should be updated?
                writeRChild iL iM'
            iM'' <- rSpNode <$> readFront dataSM iM'
            inner iM'' iM' iR
          _ -> do
            -- assemble
            iRootL <- GM.read lrs 0
            iRootR <- GM.read lrs 1
            done iM nodeM iRootL iRootR iL iR
            -- return
            let !comparison = cmpF (keySpNode nodeM)
            return (iM, comparison)

  inner i0 undefSplayIndex undefSplayIndex
  where
    done iM nodeM iRootL iRootR iL iR = do
      let !_ = assert (not (nullSplayIndex iM)) "null node after spalying?"
      let !iML = lSpNode nodeM
      let !iMR = rSpNode nodeM
      unless (nullSplayIndex iL) $ do
        writeRChild iL iML
        writeLChild iM iRootL
      unless (nullSplayIndex iR) $ do
        writeLChild iR iMR
        writeRChild iM iRootR
    writeLChild iParent iChild = do
      let !_ = assert (not (nullSplayIndex iParent)) "null parent"
      -- TODO: more efficient update?
      modifyFront dataSM (\node -> node {lSpNode = iChild}) iParent
    writeRChild iParent iChild = do
      let !_ = assert (not (nullSplayIndex iParent)) "null parent"
      -- TODO: more efficient update?
      modifyFront dataSM (\node -> node {rSpNode = iChild}) iParent

unsafeWriteSM :: (HasCallStack, U.Unbox k, U.Unbox v, PrimMonad m) => SplayMap k v (PrimState m) -> SplayIndex -> v -> m ()
unsafeWriteSM SplayMap {..} i v = do
  modifyFront dataSM (\node -> node {valSpNode = v}) i

-- | Amortized \(O(\log N)\).
pushRootSM :: (HasCallStack, U.Unbox k, U.Unbox v, PrimMonad m) => SplayMap k v (PrimState m) -> SplayNode k v -> m ()
pushRootSM SplayMap {..} node = do
  pushBack dataSM node
  len <- lengthBuffer dataSM
  GM.write rootSM 0 (len - 1)

-- | Amortized \(O(\log N)\). Removes the current root.
--
-- TODO: Less splaying
popRootSM :: (HasCallStack, Ord k, U.Unbox k, U.Unbox v, PrimMonad m) => SplayMap k v (PrimState m) -> m (SplayNode k v)
popRootSM sm@SplayMap {..} = do
  root <- GM.read rootSM 0
  node <- readFront dataSM root

  -- merge the children into one.
  root' <- case (lSpNode node, rSpNode node) of
    (-1, -1) -> return undefSplayIndex
    (!l, -1) -> return l
    (-1, !r) -> return r
    (!l, !r) -> do
      rl <- lSpNode <$> readFront dataSM r
      if nullSplayIndex rl
        then do
          -- Move @l@ to @rl@, which is null.
          --
          --   root        root
          --   / \           \
          --  l   r  -->      r
          --     /           / \
          --    XX          l   ..
          modifyFront dataSM (\nodeR -> nodeR {lSpNode = l}) r
          -- @r@ is the new root:
          return r
        else do
          lr <- rSpNode <$> readFront dataSM l
          if nullSplayIndex lr
            then do
              -- @rl@ is null, so move @r@ to @lr@:
              --      root              root
              --     /    \            /
              --    l      r  -->     l
              --   / \    / \        / \
              -- ..  XX  rl  ..    ..   r
              --                       / \
              --                     rl   ..
              modifyFront dataSM (\nodeL -> nodeL {rSpNode = r}) l
            else do
              -- Make @rl@ null if it's non-null:
              --      (i) splay rightmost       (ii) modify children
              --      root           root                 root
              --     /    \         /    \                /
              --    l      r  -->  l     rLMost  -->     l
              --   / \    / \     / \   / \             / \
              -- ..  lr  rl  .. ..  lr XX  ..         ..  rLMost
              --                                         /  \
              --                                        lr    ..
              -- (i)
              (!rLMost, !_) <- splayBySM sm (const LT) r
              -- (ii)
              modifyFront dataSM (\nodeR -> nodeR {lSpNode = lr}) rLMost
              modifyFront dataSM (\nodeL -> nodeL {rSpNode = rLMost}) l
          -- @root' = l@
          return l

  -- now the tree looks like this:
  --
  --     root
  --      |
  --     root'
  --     /  \
  --
  -- let's remove the old @root@.
  len <- lengthSM sm
  if root == len - 1
    then do
      -- FIXME: this case seems to be too rare. not efficient as the other case does splaying.
      -- the old root is at the end of the array; just remove it:
      GM.write rootSM 0 root'
      fromJust <$> popBack dataSM
    else do
      -- splay @len - 1@
      lastNode <- readBack dataSM 0
      let !key = keySpNode lastNode
      _ <- splayBySM sm (compare key) root'
      -- now the tree looks like this:
      --
      --        root
      --         |
      --    (len - 1)
      --     ..   ..
      --     root'
      -- swap @len - 1@ and the old @root@. remove @len - 1@.
      swapFront dataSM root (len - 1)
      fromJust <$> popBack dataSM

-- | Amortized \(O(\log N)\).
lookupSM :: (HasCallStack, Ord k, U.Unbox k, U.Unbox v, PrimMonad m) => SplayMap k v (PrimState m) -> k -> m (Maybe (k, v))
lookupSM sm@SplayMap {..} k = do
  root <- GM.read rootSM 0
  if nullSplayIndex root
    then return Nothing
    else do
      -- splay and lift up the closest node to the root
      (!root', !ordering) <- splayBySM sm (compare k) root
      GM.write rootSM 0 root'
      if ordering == EQ
        then do
          node <- readFront dataSM root'
          -- strict evaluation makes sense?
          let !k = keySpNode node
          let !v = valSpNode node
          return $ Just (k, v)
        else return Nothing

-- | Amortized \(O(\log N)\). Returns old value with the same key if there is.
--
-- TODO: consider making a multiset with splay tree?
insertSM :: (HasCallStack, Ord k, U.Unbox k, U.Unbox v, PrimMonad m) => SplayMap k v (PrimState m) -> k -> v -> m (Maybe v)
insertSM sm@SplayMap {..} k v = do
  root <- GM.read rootSM 0
  if nullSplayIndex root
    then do
      pushRootSM sm $ SplayNode undefSplayIndex undefSplayIndex k v
      return Nothing
    else do
      -- splay and lift up the closest node to the root
      (!root', !ordering) <- splayBySM sm (compare k) root
      -- insert or overwrite the root:
      case ordering of
        EQ -> do
          -- overwrite the existing node
          old <- valSpNode <$> readFront dataSM root'
          modifyFront dataSM (\node -> node {valSpNode = v}) root'
          GM.write rootSM 0 root'
          return $ Just old
        LT -> do
          -- insert
          l <- lSpNode <$> readFront dataSM root'
          let !r = root'
          modifyFront dataSM (\node -> node {lSpNode = undefSplayIndex}) root'
          pushRootSM sm $ SplayNode l r k v
          return Nothing
        GT -> do
          -- insert
          let !l = root'
          r <- rSpNode <$> readFront dataSM root'
          modifyFront dataSM (\node -> node {rSpNode = undefSplayIndex}) root'
          pushRootSM sm $ SplayNode l r k v
          return Nothing

insertSM_ :: (HasCallStack, Ord k, U.Unbox k, U.Unbox v, PrimMonad m) => SplayMap k v (PrimState m) -> k -> v -> m ()
insertSM_ sm k v = void $ insertSM sm k v

-- | Amortized \(O(\log N)\).
deleteSM :: (HasCallStack, Ord k, U.Unbox k, U.Unbox v, PrimMonad m) => SplayMap k v (PrimState m) -> k -> m (Maybe (SplayNode k v))
deleteSM sm@SplayMap {..} k = do
  root <- GM.read rootSM 0
  if nullSplayIndex root
    then return Nothing
    else do
      -- splay and lift up the closest node to the root
      (!newRoot, !ordering) <- splayBySM sm (compare k) root
      GM.write rootSM 0 newRoot
      if ordering == EQ
        then Just <$> popRootSM sm
        else return Nothing

memberSM :: (HasCallStack, Ord k, U.Unbox k, U.Unbox v, PrimMonad m) => SplayMap k v (PrimState m) -> k -> m Bool
memberSM sm@SplayMap {..} k = do
  root <- GM.read rootSM 0
  if nullSplayIndex root
    then return False
    else do
      (!newRoot, !ordering) <- splayBySM sm (compare k) root
      GM.write rootSM 0 newRoot
      return $ ordering == EQ

-- | Amortized \(O(\log N)\). Internal use only.
_lookupWithLESM :: (HasCallStack, U.Unbox k, U.Unbox v, PrimMonad m) => SplayMap k v (PrimState m) -> (k -> Ordering) -> m (Maybe (k, v))
_lookupWithLESM sm@SplayMap {..} cmpF = do
  -- TODO: consider using MaybeT?
  root <- GM.read rootSM 0
  if nullSplayIndex root
    then return Nothing
    else do
      -- splay finds the nearest node?
      (!root', !order) <- splayBySM sm cmpF root
      GM.write rootSM 0 root'
      if order == EQ || order == GT
        then do
          rootData <- readFront dataSM root'
          -- strict evaluation makes sense?
          let !k = keySpNode rootData
          let !v = valSpNode rootData
          return $ Just (k, v)
        else do
          l <- lSpNode <$> readFront dataSM root'
          if nullSplayIndex l
            then return Nothing
            else do
              -- FIXME:
              (!i, !_) <- splayBySM sm (const GT) l
              modifyFront dataSM (\node -> node {lSpNode = i}) root'
              node <- readFront dataSM i
              let !k' = keySpNode node
              let !v' = valSpNode node
              -- when (k' > k) $ error "unreachable"
              return $ Just (k', v')

-- | Amortized \(O(\log N)\).
lookupLESM :: (HasCallStack, Ord k, U.Unbox k, U.Unbox v, PrimMonad m) => SplayMap k v (PrimState m) -> k -> m (Maybe (k, v))
lookupLESM sm k = _lookupWithLESM sm (compare k)

-- | Amortized \(O(\log N)\).
lookupLTSM :: (HasCallStack, Ord k, U.Unbox k, U.Unbox v, PrimMonad m) => SplayMap k v (PrimState m) -> k -> m (Maybe (k, v))
lookupLTSM sm k = _lookupWithLESM sm (\k' -> if k < k' then LT else GT)

-- | Amortized \(O(\log N)\).
_lookupWithGESM :: (HasCallStack, U.Unbox k, U.Unbox v, PrimMonad m) => SplayMap k v (PrimState m) -> (k -> Ordering) -> m (Maybe (k, v))
_lookupWithGESM sm@SplayMap {..} cmpF = do
  -- TODO: consider using MaybeT?
  root <- GM.read rootSM 0
  if nullSplayIndex root
    then return Nothing
    else do
      (!root', !order) <- splayBySM sm cmpF root
      GM.write rootSM 0 root'
      if order == EQ || order == LT
        then do
          rootData <- readFront dataSM root'
          -- strict evaluation makes sense?
          let !k = keySpNode rootData
          let !v = valSpNode rootData
          return $ Just (k, v)
        else do
          r <- rSpNode <$> readFront dataSM root'
          if nullSplayIndex r
            then return Nothing
            else do
              -- FIXME:
              (!i, !_) <- splayBySM sm (const LT) r
              modifyFront dataSM (\node -> node {rSpNode = i}) root'
              node <- readFront dataSM i
              let !k' = keySpNode node
              let !v' = valSpNode node
              -- when (k' < k) error "unreachable"
              return $ Just (k', v')

-- | Amortized \(O(\log N)\).
lookupGESM :: (HasCallStack, Ord k, U.Unbox k, U.Unbox v, PrimMonad m) => SplayMap k v (PrimState m) -> k -> m (Maybe (k, v))
lookupGESM sm k = _lookupWithGESM sm (compare k)

-- | Amortized \(O(\log N)\).
lookupGTSM :: (HasCallStack, Ord k, U.Unbox k, U.Unbox v, PrimMonad m) => SplayMap k v (PrimState m) -> k -> m (Maybe (k, v))
lookupGTSM sm k = _lookupWithGESM sm (\k' -> if k > k' then GT else LT)

-- | Returns @(key, value)@ pairs sorted by the keys in ascending order.
dfsSM :: (HasCallStack, U.Unbox k, U.Unbox v, PrimMonad m) => SplayMap k v (PrimState m) -> m (U.Vector (k, v))
dfsSM sm@SplayMap {..} = do
  buf <- newBuffer =<< lengthSM sm
  root <- GM.read rootSM 0
  flip fix root $ \loop i -> do
    unless (nullSplayIndex i) $ do
      node <- readFront dataSM i
      unless (nullSplayIndex (lSpNode node)) $ do
        loop $ lSpNode node
      pushBack buf (keySpNode node, valSpNode node)
      unless (nullSplayIndex (rSpNode node)) $ do
        loop $ rSpNode node
  unsafeFreezeBuffer buf
