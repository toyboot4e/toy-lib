{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | A mutable splay tree-based sequence.
module Data.SplaySeq where

import Control.Exception (assert)
import Control.Monad (unless, when)
import Control.Monad.Extra (whenJust, whenM)
import Control.Monad.Fix (fix)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bits
import Data.Core.SemigroupAction
import Data.Pool
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Debug.Trace
import GHC.Stack (HasCallStack)
import ToyLib.Debug

-- TODO: always return root as the first or the second element.

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
data SplaySeq s v a = SplaySeq
  { -- | The maximum number of elements.
    capacitySS :: {-# UNPACK #-} !Int,
    -- | Pool for free slot handing.
    freeSS :: !(Pool s ()),
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
    aggSS :: !(UM.MVector s v),
    -- | Decomposed node data storage: reversed flag of children.
    -- TODO: use Bit?
    revSS :: !(UM.MVector s Bool),
    -- | Decomposed node data storage: lazily propagated semigroup action.
    actSS :: !(UM.MVector s a)
  }

-- | \(O(N)\) Creates a new `SplaySeq` of capacity @n@.
{-# INLINE newSS #-}
newSS :: (U.Unbox v, U.Unbox a, PrimMonad m) => Int -> m (SplaySeq (PrimState m) v a)
newSS n = do
  freeSS <- newPool n
  lSS <- UM.unsafeNew n
  rSS <- UM.unsafeNew n
  pSS <- UM.unsafeNew n
  sSS <- UM.unsafeNew n
  vSS <- UM.unsafeNew n
  aggSS <- UM.unsafeNew n
  revSS <- UM.unsafeNew n
  actSS <- UM.unsafeNew n
  return $ SplaySeq {capacitySS = n, ..}

-- | \(O(1)\) Returns the number of elements stored. Requires monad for tracking the state.
{-# INLINE sizeSS #-}
sizeSS :: (PrimMonad m) => SplaySeq (PrimState m) v a -> m Int
sizeSS = sizePool . freeSS

-- | \(O(1)\) Returns the number of elements in a sequence.
{-# INLINE seqSizeSS #-}
seqSizeSS :: (HasCallStack, PrimMonad m) => SplaySeq (PrimState m) v a -> Int -> m Int
seqSizeSS seq root
  | nullSI root = return 0
  | otherwise = GM.read (sSS seq) root

-- * Allocation

-- | \(O(1)\) Allocates a new node as a single element sequence.
allocNodeSS :: (HasCallStack, PrimMonad m, U.Unbox v, Monoid a, U.Unbox a) => SplaySeq (PrimState m) v a -> v -> m SplayIndex
allocNodeSS SplaySeq {..} !v = do
  i <- allocPool freeSS ()
  GM.write lSS i undefSI
  GM.write rSS i undefSI
  GM.write pSS i undefSI
  GM.write sSS i (1 :: Int)
  GM.write vSS i v
  GM.write aggSS i v
  GM.write revSS i False
  GM.write actSS i mempty
  return i

-- | \(O(N)\) Allocates a new sequence, internally as a binary tree from the bottom to the top.
allocSeqSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a) => SplaySeq (PrimState m) v a -> U.Vector v -> m SplayIndex
allocSeqSS seq@SplaySeq {..} !xs = do
  -- [l, r)
  let inner l r
        | l >= r = return undefSI
        | l + 1 == r = allocNodeSS seq $ xs G.! l
        | otherwise = do
            let !m = (l + r) `div` 2
            rootL <- inner l m
            root <- allocNodeSS seq (xs G.! m)
            rootR <- inner (m + 1) r
            unless (nullSI rootL) $ do
              GM.write lSS root rootL
              GM.write pSS rootL root
            unless (nullSI rootR) $ do
              GM.write rSS root rootR
              GM.write pSS rootR root
            updateNodeSS seq root
            return root
  inner 0 (G.length xs)

-- | \(O(1)\) Frees a node.
freeNodeSS :: (HasCallStack, PrimMonad m) => SplaySeq (PrimState m) v a -> SplayIndex -> m ()
freeNodeSS = deallocPool . freeSS

-- | \(O(N)\) Frees a subtree.
freeSubtreeSS :: (HasCallStack, PrimMonad m) => SplaySeq (PrimState m) v a -> SplayIndex -> m ()
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

-- | \(O(1)\) Asserts the node is the root.
{-# INLINE assertRootSS #-}
assertRootSS :: (HasCallStack, PrimMonad m) => SplaySeq (PrimState m) v a -> SplayIndex -> m ()
assertRootSS SplaySeq {..} root = dbgM $ do
  let !_ = assert (not (nullSI root)) "null as a root"
  p <- GM.read pSS root
  let !_ = assert (nullSI p) "not a root"
  return ()

-- * API

-- | Amortized \(O(\log N)\). Reads a kth node's value.
{-# INLINE readSS #-}
readSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SemigroupAction a v, Eq a) => SplaySeq (PrimState m) v a -> SplayIndex -> Int -> m (SplayIndex, v)
readSS seq@SplaySeq {..} root k = do
  assertRootSS seq root
  root' <- splayKthSS seq root k
  (root',) <$> GM.read vSS root'

-- TODO: freezeSS (get_all)

-- | Amortized \(O(\log N)\). Writes a kth node's value.
{-# INLINE writeSS #-}
writeSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SemigroupAction a v, Eq a) => SplaySeq (PrimState m) v a -> SplayIndex -> Int -> v -> m SplayIndex
writeSS seq root k v = do
  assertRootSS seq root
  root' <- splayKthSS seq root k
  writeNodeSS seq root' v
  return root'

-- | Amortized \(O(\log N)\). Modifies a kth node's value.
{-# INLINE modifySS #-}
modifySS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SemigroupAction a v, Eq a) => SplaySeq (PrimState m) v a -> SplayIndex -> (v -> v) -> Int -> m SplayIndex
modifySS seq root f k = do
  assertRootSS seq root
  root' <- splayKthSS seq root k
  modifyNodeSS seq f root'
  return root'

-- | Amortized \(O(\log N)\). Exchanges a kth node's value.
{-# INLINE exchangeSS #-}
exchangeSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SemigroupAction a v, Eq a) => SplaySeq (PrimState m) v a -> SplayIndex -> Int -> v -> m (v, SplayIndex)
exchangeSS seq root k v = do
  assertRootSS seq root
  root' <- splayKthSS seq root k
  res <- exchangeNodeSS seq root' v
  return (res, root')

-- | Amortized \(O(\log N)\). Folds an interval @[l, r]@.
{-# INLINE foldSS #-}
foldSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SemigroupAction a v, Eq a) => SplaySeq (PrimState m) v a -> SplayIndex -> Int -> Int -> m (v, SplayIndex)
foldSS seq@SplaySeq {..} root l r
  | l > r = return (mempty, root)
  | otherwise = do
      size <- GM.read sSS root
      -- TODO: foldMaySS
      let !_ = assert (0 <= l && l <= r && r < size) "invalid interval"
      assertRootSS seq root
      target <- captureSS seq root l (r + 1)
      res <- GM.read aggSS target
      splaySS seq target True
      return (res, target)

-- | Amortized \(O(\log N)\). Folds the whole sequence.
{-# INLINE foldAllSS #-}
foldAllSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SemigroupAction a v, Eq a) => SplaySeq (PrimState m) v a -> SplayIndex -> m v
foldAllSS seq@SplaySeq {..} root = do
  assertRootSS seq root
  GM.read aggSS root

-- | Amortized \(O(\log N)\).
{-# INLINE sactSS #-}
sactSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SemigroupAction a v, Eq a) => SplaySeq (PrimState m) v a -> SplayIndex -> Int -> Int -> s -> m SplayIndex
sactSS seq@SplaySeq {..} root l r act = do
  assertRootSS seq root
  -- GM.read aggSS root
  return root

-- | Amortised \(O(\log N)\).
{-# INLINE reverseSS #-}
reverseSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SemigroupAction a v, Eq a) => SplaySeq (PrimState m) v a -> SplayIndex -> Int -> Int -> m SplayIndex
reverseSS seq root0 l r
  | l > r = return root0
  | otherwise = do
      root' <- captureSS seq root0 l (r + 1)
      reverseNodeSS seq root'
      splaySS seq root' True
      return root'

-- | Amortized \(O(\log N)\). Inserts a node at @k@.
{-# INLINE insertSS #-}
insertSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SemigroupAction a v, Eq a) => SplaySeq (PrimState m) v a -> SplayIndex -> Int -> v -> m SplayIndex
insertSS seq root k v = do
  (!l, !r) <- splitAtSS seq root k
  node <- allocNodeSS seq v
  merge3SS seq l node r

-- | Amortized \(O(\log N)\). Deletes a node at @k@.
{-# INLINE deleteSS #-}
deleteSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SemigroupAction a v, Eq a) => SplaySeq (PrimState m) v a -> SplayIndex -> Int -> m SplayIndex
deleteSS seq root i = do
  (!l, !m, !r) <- split3SS seq root i (i + 1)
  freeNodeSS seq m
  root' <- mergeSS seq l r
  return root'

-- | Amortized \(O(\log N)\). Bisection method over the sequence. Partition point. Note that The
-- user function is run over each node, not fold of an interval.
{-# INLINE bisectLSS #-}
bisectLSS :: (Show v, HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SemigroupAction a v, Eq a) => SplaySeq (PrimState m) v a -> SplayIndex -> (v -> Bool) -> m (SplayIndex, Maybe SplayIndex)
bisectLSS seq@SplaySeq {..} root0 check = do
  let inner root parent lastYes
        -- FIXME
        | nullSI root && nullSI lastYes = return (parent, Nothing)
        | nullSI root = return (parent, Just lastYes)
        | otherwise = do
            propNodeSS seq root
            v <- GM.read vSS root
            if check v
              then do
                r <- GM.read rSS root
                inner r root root
              else do
                l <- GM.read lSS root
                inner l root lastYes

  (!root', !found) <- inner root0 undefSI undefSI
  unless (nullSI root') $ do
    splaySS seq root' True
  return (root', found)

-- * Self-balancing methods (internals)

-- | Amortized \(O(\log N)\). Rotates the node. Propagation and updates are done outside of the
-- function.
rotateSS :: (HasCallStack, PrimMonad m) => SplaySeq (PrimState m) v a -> SplayIndex -> m ()
rotateSS SplaySeq {..} !i = do
  p <- GM.read pSS i
  pl <- GM.read lSS p

  c <-
    if pl == i
      then do
        --   p       i
        --  /         \
        -- i     ->    p
        --  \         /
        --   r       r
        r <- GM.exchange rSS i p
        GM.write lSS p r
        return r
      else do
        -- p          i
        --  \        /
        --   i  ->  p
        --  /        \
        -- l          l
        l <- GM.exchange lSS i p
        GM.write rSS p l
        return l

  pp <- GM.read pSS p
  unless (nullSI pp) $ do
    --   pp      pp
    --  /    -> /
    -- p       i
    GM.modify lSS (\ppl -> if ppl == p then i else ppl) pp
    --   pp       pp
    --     \  ->    \
    --      p        i
    GM.modify rSS (\ppr -> if ppr == p then i else ppr) pp

  -- set parents
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
splaySS :: (HasCallStack, PrimMonad m, U.Unbox v, Monoid v, Monoid a, U.Unbox a, SemigroupAction a v, Eq a) => SplaySeq (PrimState m) v a -> SplayIndex -> Bool -> m ()
splaySS seq@SplaySeq {..} i doneParentProp = do
  if doneParentProp
    then propNodeSS seq i
    else propNodeFromRootSS seq i

  fix $ \loop -> do
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
          if pl == i && ppl == p || pr == i && ppr == p
            then do
              -- same direction twice
              rotateSS seq p
              rotateSS seq i
            else do
              rotateSS seq i
              rotateSS seq i
          updateNodeSS seq pp
          updateNodeSS seq p
      loop

  updateNodeSS seq i

-- | Amortized \(O(\log N)\). Finds @k@ th node from the left and splays it.
-- Returns the new root.
splayKthSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SemigroupAction a v, Eq a) => SplaySeq (PrimState m) v a -> SplayIndex -> Int -> m SplayIndex
splayKthSS seq@SplaySeq {..} root0 k0 = do
  size <- GM.read sSS root0
  let !_ = assert (0 <= k0 && k0 < size) "no kth element in the sequence"

  let inner root k = do
        propNodeSS seq root
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

-- * Node operations (internals)

-- | \(O(1)\) Recomputes the node size and the monoid aggregation.
{-# INLINE updateNodeSS #-}
updateNodeSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a) => SplaySeq (PrimState m) v a -> SplayIndex -> m ()
updateNodeSS SplaySeq {..} i = do
  l <- GM.read lSS i
  r <- GM.read rSS i
  v <- GM.read vSS i
  (!sizeL, !aggL) <- if nullSI l then return (0, mempty) else (,) <$> GM.read sSS l <*> GM.read aggSS l
  (!sizeR, !aggR) <- if nullSI r then return (0, mempty) else (,) <$> GM.read sSS r <*> GM.read aggSS r
  GM.write sSS i $! sizeL + 1 + sizeR
  GM.write aggSS i $! aggL <> v <> aggR

-- | \(O(1)\) Writes the monoid.
--
-- = Prerequisties
-- The node is a root (it's splayed).
{-# INLINE writeNodeSS #-}
writeNodeSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a) => SplaySeq (PrimState m) v a -> SplayIndex -> v -> m ()
writeNodeSS seq@SplaySeq {..} root v = do
  assertRootSS seq root
  GM.write vSS root v
  updateNodeSS seq root

-- | \(O(1)\) Modifies the monoid.
--
-- = Prerequisties
-- The node is a root (it's splayed).
{-# INLINE modifyNodeSS #-}
modifyNodeSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a) => SplaySeq (PrimState m) v a -> (v -> v) -> SplayIndex -> m ()
modifyNodeSS seq@SplaySeq {..} f root = do
  assertRootSS seq root
  GM.modify vSS f root
  updateNodeSS seq root

-- | \(O(1)\) Modifies the monoid.
--
-- = Prerequisties
-- The node is a root (it's splayed).
{-# INLINE exchangeNodeSS #-}
exchangeNodeSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a) => SplaySeq (PrimState m) v a -> SplayIndex -> v -> m v
exchangeNodeSS seq@SplaySeq {..} root v = do
  assertRootSS seq root
  res <- GM.exchange vSS root v
  updateNodeSS seq root
  return res

-- | \(O(1)\) Reverses left and right children.
{-# INLINE swapLrNodeSS #-}
swapLrNodeSS :: (HasCallStack, PrimMonad m) => SplaySeq (PrimState m) v a -> SplayIndex -> m ()
swapLrNodeSS SplaySeq {..} i = do
  l <- GM.read lSS i
  r <- GM.read rSS i
  GM.write lSS i r
  GM.write rSS i l

-- | \(O(1)\) Reverses left and right children, recursively and lazily.
{-# INLINE reverseNodeSS #-}
reverseNodeSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a) => SplaySeq (PrimState m) v a -> SplayIndex -> m ()
reverseNodeSS seq@SplaySeq {..} i = do
  swapLrNodeSS seq i
  -- reverse or quit
  GM.modify revSS (xor True) i

-- | Amortized \(O(\log N)\). Propgates at a node.
{-# INLINE propNodeSS #-}
propNodeSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SemigroupAction a v, Eq a) => SplaySeq (PrimState m) v a -> SplayIndex -> m ()
propNodeSS seq@SplaySeq {..} i = do
  -- action
  act <- GM.exchange actSS i mempty
  when (act /= mempty) $ do
    l <- GM.read lSS i
    unless (nullSI l) $ do
      sactNodeSS seq l act
    r <- GM.read lSS i
    unless (nullSI r) $ do
      sactNodeSS seq r act

  -- reverse
  whenM (GM.exchange revSS i False) $ do
    l <- GM.read lSS i
    unless (nullSI l) $ do
      -- propagate new reverse or cancel:
      GM.modify revSS (xor True) l
      swapLrNodeSS seq l
    r <- GM.read rSS i
    unless (nullSI r) $ do
      -- propagate new reverse or cancel:
      GM.modify revSS (xor True) r
      swapLrNodeSS seq r

-- | Amortized \(O(\log N)\). Propagetes from the root to the given node.
{-# INLINE propNodeFromRootSS #-}
propNodeFromRootSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SemigroupAction a v, Eq a) => SplaySeq (PrimState m) v a -> SplayIndex -> m ()
propNodeFromRootSS seq@SplaySeq {..} i0 = inner i0
  where
    inner i = do
      p <- GM.read pSS i
      unless (nullSI p) $ do
        inner p
      propNodeSS seq i

-- | Amortized \(O(\log N)\). Propgates at a node.
{-# INLINE sactNodeSS #-}
sactNodeSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SemigroupAction a v, Eq a) => SplaySeq (PrimState m) v a -> SplayIndex -> a -> m ()
sactNodeSS SplaySeq {..} i act = do
  -- TODO: consider using `SegmentTreeAction`. no, it's `SemigroupActionWithLength`.
  GM.modify vSS (act `sact`) i
  GM.modify aggSS (act `sact`) i
  GM.modify actSS (act <>) i

-- * Split / merge

-- | Amortized \(O(\log N)\). Merges two nodes. It's implemented a reverse operation of `splitSS`.
mergeSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SemigroupAction a v, Eq a) => SplaySeq (PrimState m) v a -> SplayIndex -> SplayIndex -> m SplayIndex
mergeSS seq@SplaySeq {..} l r
  | nullSI l = return r
  | nullSI r = return l
  | otherwise = do
      lp <- GM.read pSS l
      rp <- GM.read pSS r
      let !_ = assert (not (nullSI lp)) "left root is null"
      let !_ = assert (not (nullSI rp)) "right root is null"
      -- TODO: what is this?
      r' <- splayKthSS seq r 0 -- propagateD
      GM.write lSS r l
      GM.write pSS l r'
      updateNodeSS seq r'
      return r'

-- | Amortized \(O(\log N)\)
merge3SS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SemigroupAction a v, Eq a) => SplaySeq (PrimState m) v a -> SplayIndex -> SplayIndex -> SplayIndex -> m SplayIndex
merge3SS seq l m r = do
  node <- mergeSS seq l m
  root <- mergeSS seq node r
  return root

-- | Amortized \(O(\log N)\). Splits a sequneces into two nodes. It's implemented a reverse
-- operation of `splitSS`.
splitAtSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SemigroupAction a v, Eq a) => SplaySeq (PrimState m) v a -> SplayIndex -> Int -> m (SplayIndex, SplayIndex)
splitAtSS seq@SplaySeq {..} root k
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

-- | Amortized \(O(\log N)\). Splits a tree into three.
split3SS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SemigroupAction a v, Eq a) => SplaySeq (PrimState m) v a -> SplayIndex -> Int -> Int -> m (SplayIndex, SplayIndex, SplayIndex)
split3SS seq root l r = do
  (!root', !nodeR) <- splitAtSS seq root r
  (!nodeL, !nodeM) <- splitAtSS seq root' l
  return (nodeL, nodeM, nodeR)

-- | Amortized \(O(\log N)\). Captures a subtree of [l, r). Be sure it's half-open interval! Be sure
-- to splay the new root after call.
captureSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SemigroupAction a v, Eq a) => SplaySeq (PrimState m) v a -> SplayIndex -> Int -> Int -> m SplayIndex
captureSS seq@SplaySeq {..} root l r
  | l == 0 = do
      size <- GM.read sSS root
      if r == size
        then return root
        else do
          root' <- splayKthSS seq root r
          GM.read lSS root'
  | otherwise = do
      size <- GM.read sSS root
      if r == size
        then do
          root' <- splayKthSS seq root (l - 1)
          GM.read rSS root'
        else do
          -- o--l--o--o--r--o
          --    [        )
          --             * root' (splayed)
          --          * rootL (detached from the root)
          -- \* rootL' (detached and splayed)
          --    * right(rootL'): node that corresponds to [l, r)
          root' <- splayKthSS seq root r
          rootL <- GM.read lSS root'
          -- detach `rootL` from `root'`
          GM.write pSS rootL undefSI
          rootL' <- splayKthSS seq rootL (l - 1)
          -- re-attach `rootL'` to `root'`
          GM.write pSS rootL' root'
          GM.write lSS root' rootL'
          updateNodeSS seq root'
          GM.read rSS rootL'

-- TODO: sact
