{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | A mutable splay tree-based sequence.
module Data.SplaySeq where

import Control.Exception (assert)
import Control.Monad (unless, when)
import Control.Monad.Fix (fix)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Buffer
import Data.Maybe
import Data.Pool
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Stack (HasCallStack)
import ToyLib.Debug

-- | Index of a `SplayNode` stored in a `SplayMap`.
type SplayIndex = PoolIndex

{-# INLINE undefSI #-}
undefSI :: SplayIndex
undefSI = undefPI

{-# INLINE nullSI #-}
nullSI :: SplayIndex -> Bool
nullSI = nullPI

-- TODO: hide SplayIndex from user and provide with stable root handle.

-- | Mutable, splay tree-based sequences.
--
-- = Invariants
-- - The sequence order is kept in the tree. Left children have smaller indices and right children
--   have bigger indices.
data SplaySeq s v = SplaySeq
  { -- | The maximum number of elements.
    capacitySS :: {-# UNPACK #-} !Int,
    -- | Stack of free slots in the data storage for \(O(1)\) node allocation. We could wemove it
    -- if Haskell has sum type with the niche optimization.
    --
    -- FIXME: It's not compatible with O(1) reset.
    freeSS :: !(Buffer s Int),
    -- | Decomposed node data storage: left children.
    lSS :: !(UM.MVector s SplayIndex),
    -- | Decomposed node data storage: right children.
    rSS :: !(UM.MVector s SplayIndex),
    -- | Decomposed node data storage: parents.
    pSS :: !(UM.MVector s SplayIndex),
    -- | Decomposed node data storage: subtree sizes.
    sSS :: !(UM.MVector s SplayIndex),
    -- | Decomposed node data storage: payloads.
    vSS :: !(UM.MVector s v),
    -- | Decomposed node data storage: aggregation of payloads.
    aggSS :: !(UM.MVector s v)
  }

-- | \(O(N)\) Creates a new `SplaySeq` of capacity @n@.
{-# INLINE newSS #-}
newSS :: (U.Unbox v, PrimMonad m) => Int -> m (SplaySeq (PrimState m) v)
newSS n = do
  freeSS <- generateBuffer n id
  lSS <- UM.unsafeNew n
  rSS <- UM.unsafeNew n
  pSS <- UM.unsafeNew n
  sSS <- UM.unsafeNew n
  vSS <- UM.unsafeNew n
  aggSS <- UM.unsafeNew n
  return $ SplaySeq {capacitySS = n, ..}

-- | \(O(1)\) Returns the number of elements stored. Requires monad for tracking the state.
{-# INLINE lengthSS #-}
lengthSS :: (PrimMonad m) => SplaySeq (PrimState m) v -> m Int
lengthSS SplaySeq {..} = do
  nFree <- lengthBuffer freeSS
  return $ capacitySS - nFree

-- * Allocation

-- | \(O(1)\) Allocates a new node.
allocNodeSS :: (HasCallStack, PrimMonad m, U.Unbox v) => SplaySeq (PrimState m) v -> v -> m SplayIndex
allocNodeSS seq@SplaySeq {..} !v = do
  i <- fromJust <$> popFront freeSS
  GM.write lSS i undefSI
  GM.write rSS i undefSI
  GM.write pSS i undefSI
  GM.write sSS i 1
  GM.write vSS i v
  return i

-- | \(O(N)\) Allocates a new sequence, internally as a binary tree from the bottom to the top.
allocSeqSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v) => SplaySeq (PrimState m) v -> U.Vector v -> m SplayIndex
allocSeqSS seq@SplaySeq {..} !xs = do
  -- [l, r)
  let inner l r
        | l == r = return undefSI
        | r == l + 1 = allocNodeSS seq $ xs G.! l
        | otherwise = do
            let !m = l + r `div` 2
            rootL <- inner l m
            root <- allocNodeSS seq (xs G.! m)
            rootR <- inner (m + 1) r
            GM.write lSS root rootL
            GM.write rSS root rootR
            updateNodeSS seq root
            return root
  inner 0 (G.length xs)

-- | \(d(1)\) Frees a node.
freeNodeSS :: (PrimMonad m) => SplaySeq (PrimState m) v -> SplayIndex -> m ()
freeNodeSS seq@SplaySeq {..} !i = do
  pushFront freeSS i

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

-- | Amortized \(O(\log N)\). Reads a kth node's value.
{-# INLINE readSS #-}
readSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v) => SplaySeq (PrimState m) v -> SplayIndex -> Int -> m (SplayIndex, v)
readSS seq@SplaySeq {..} root k = do
  dbgM $ do
    p <- GM.read pSS root
    let !_ = assert (nullSI p) "not a root"
    return ()
  root' <- splayKthSS seq root k
  (root',) <$> GM.read vSS root'

-- TODO: freezeSS (get_all)

-- | Amortized \(O(\log N)\). Writes a kth node's value.
{-# INLINE writeSS #-}
writeSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v) => SplaySeq (PrimState m) v -> SplayIndex -> Int -> v -> m SplayIndex
writeSS seq@SplaySeq {..} root k v = do
  dbgM $ do
    p <- GM.read pSS root
    let !_ = assert (nullSI p) "not a root"
    return ()
  root' <- splayKthSS seq root k
  writeNodeSS seq root' v
  return root'

-- * Self-balancing

-- | Amortized \(O(\log N)\). Rotates the  node. Propagation and updates are done outside of the
-- function.
rotateSS :: (PrimMonad m) => SplaySeq (PrimState m) v -> SplayIndex -> m ()
rotateSS seq@SplaySeq {..} !i = do
  p <- GM.read pSS i
  l <- GM.read lSS i
  r <- GM.read rSS i

  pp <- GM.read pSS p
  pl <- GM.read lSS p

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

-- | Amortized \(O(\log N)\). Moves a node to the root, performing self-balancing heuristic called
-- rotations.
--
-- = Prerequisites
-- Parents are already propagated.
--
-- = After call
-- The node is updated and propagated.
splaySS :: (PrimMonad m, U.Unbox v, Monoid v) => SplaySeq (PrimState m) v -> SplayIndex -> Bool -> m ()
splaySS seq@SplaySeq {..} i0 doneProp = do
  unless doneProp $ do
    propNodeFromRootSS seq i0

  flip fix i0 $ \loop i -> do
    p <- GM.read pSS i
    unless (nullSI p) $ do
      pp <- GM.read pSS p
      if nullSI pp
        then do
          rotateSS seq i
          updateNodeSS seq p
          return ()
        else do
          pl <- GM.read lSS p
          pr <- GM.read rSS p
          ppl <- GM.read lSS pp
          ppr <- GM.read rSS pp
          let !same = pl == i && ppl == p || pr == i && ppr == p
          if same
            then do
              rotateSS seq p
              rotateSS seq i
            else do
              rotateSS seq i
              rotateSS seq i
          updateNodeSS seq pp
          updateNodeSS seq p
      loop i

  updateNodeSS seq i0

-- | Amortized \(O(\log N)\). Finds @k@ node and splays it.
-- Returns the new root.
splayKthSS :: (PrimMonad m, Monoid v, U.Unbox v) => SplaySeq (PrimState m) v -> SplayIndex -> Int -> m SplayIndex
splayKthSS seq@SplaySeq {..} root0 k0 = do
  size <- GM.read sSS root0
  let !_ = assert (0 <= k0 && k0 < size) "size mismatch"

  let inner root k = do
        propNode seq root
        l <- GM.read lSS root
        -- The number of left children = the node's index counting from the leftmost.
        sizeL <- if nullSI l then return 0 else GM.read sSS l
        case compare k sizeL of
          EQ -> return root
          LT -> inner l k
          GT -> do
            r <- GM.read rSS root
            inner r (k - (sizeL + 1))

  target <- inner root0 k0
  splaySS seq target True
  return target

-- * Node operations

-- | \(O(1)\) Recomputes the node size and the monoid aggregation.
{-# INLINE updateNodeSS #-}
updateNodeSS :: (PrimMonad m, Monoid v, U.Unbox v) => SplaySeq (PrimState m) v -> SplayIndex -> m ()
updateNodeSS seq@SplaySeq {..} i = do
  l <- GM.read lSS i
  r <- GM.read rSS i
  (!sizeL, !aggL) <- if nullSI l then return (0, mempty) else (,) <$> GM.read sSS l <*> GM.read aggSS l
  (!sizeR, !aggR) <- if nullSI r then return (0, mempty) else (,) <$> GM.read sSS r <*> GM.read aggSS r
  GM.write sSS i $! sizeL + 1 + sizeR
  GM.write aggSS i $! aggL <> aggR

-- | \(O(1)\) Write the monoid.
--
-- = Prerequisties
-- The node is a root.
{-# INLINE writeNodeSS #-}
writeNodeSS :: (PrimMonad m, Monoid v, U.Unbox v) => SplaySeq (PrimState m) v -> SplayIndex -> v -> m ()
writeNodeSS seq@SplaySeq {..} root v = do
  GM.write vSS root v
  updateNodeSS seq root

{-# INLINE propNode #-}
propNode :: (PrimMonad m) => SplaySeq (PrimState m) v -> SplayIndex -> m ()
propNode seq@SplaySeq {..} i = do
  return ()

{-# INLINE propNodeFromRootSS #-}
propNodeFromRootSS :: (PrimMonad m) => SplaySeq (PrimState m) v -> SplayIndex -> m ()
propNodeFromRootSS seq@SplaySeq {..} i = do
  return ()

-- * Tree operations

-- | Amortized \(O(\log N)\)
mergeSS :: (PrimMonad m, Monoid v, U.Unbox v) => SplaySeq (PrimState m) v -> SplayIndex -> SplayIndex -> m SplayIndex
mergeSS seq@SplaySeq {..} l r
  | nullSI l = return r
  | nullSI r = return l
  | otherwise = do
      lp <- GM.read pSS l
      rp <- GM.read pSS r
      let !_ = assert (nullSI lp) "left root is null"
      let !_ = assert (nullSI rp) "right root is null"
      -- TODO: what is this?
      r' <- splayKthSS seq r 0 -- propagateD
      GM.write lSS r l
      GM.write pSS l r'
      updateNodeSS seq r'
      return r'

-- | Amortized \(O(\log N)\)
splitSS :: (PrimMonad m, Monoid v, U.Unbox v) => SplaySeq (PrimState m) v -> SplayIndex -> Int -> m (SplayIndex, SplayIndex)
splitSS seq@SplaySeq {..} root k
  | k == 0 = return (undefSI, root)
  | otherwise = do
      p <- GM.read pSS root
      let !_ = assert (nullSI p) "split: not given root"
      size <- GM.read sSS root
      if k == size
        then return (root, undefSI)
        else do
          root' <- splayKthSS seq root (k - 1)
          r <- GM.exchange rSS root' undefSI
          GM.write pSS r undefSI
          updateNodeSS seq root'
          return (root', r)

-- | Amortized \(O(\log N)\)Returns a node that corresponds to [l, r). Be sure to splay the new root
-- after call.
gotoSS :: (PrimMonad m, Monoid v, U.Unbox v) => SplaySeq (PrimState m) v -> SplayIndex -> Int -> Int -> m (SplayIndex, SplayIndex)
gotoSS seq@SplaySeq {..} root l r
  | l == 0 = do
      size <- GM.read sSS root
      if r == size
        then return (root, root)
        else do
          root' <- splayKthSS seq root r
          (root',) <$> GM.read lSS root'
  | otherwise = do
      size <- GM.read sSS root
      if r == size
        then do
          root' <- splayKthSS seq root (l - 1)
          (root',) <$> GM.read rSS root'
        else do
          root' <- splayKthSS seq root r
          rootL <- GM.read lSS root'
          GM.write pSS rootL undefSI
          root'' <- splayKthSS seq root' (l - 1)
          GM.write pSS rootL root''
          GM.write lSS root'' rootL
          updateNodeSS seq root''
          (root'',) <$> GM.read rSS root''
