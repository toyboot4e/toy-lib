{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | A mutable splay tree-based sequence.
module Data.SplaySeq.Raw where

import Control.Exception (assert)
import Control.Monad (unless, when)
import Control.Monad.Extra (whenM)
import Control.Monad.Fix (fix)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bit
import Data.Bits
import Data.Coerce
import Data.Core.SegmentAction
import Data.Pool
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Stack (HasCallStack)
import ToyLib.Debug

-- | Strongly typed index of nodes in a `SplayMap`.
type SplayIndex = PoolIndex

{-# INLINE undefSI #-}
undefSI :: SplayIndex
undefSI = undefPI

{-# INLINE nullSI #-}
nullSI :: SplayIndex -> Bool
nullSI = nullPI

-- | Mutable, splay tree-based sequences.
--
-- = Invariants
-- - The sequence order is kept in the tree before and after the splaying operation. Left children
-- have smaller indices and right children have bigger indices.
--
-- = I don't need lazily propagated actions
-- Well, use `RawSplaySeq s v v`. It allocates an unused vector, but doesn't duplicate the source
-- code anyways.
data RawSplaySeq s v a = RawSplaySeq
  { -- | The maximum number of elements.
    capacityRSS :: {-# UNPACK #-} !Int,
    -- | Pool for free slot handing.
    freeRSS :: !(Pool s ()),
    -- | Decomposed node data storage: left children.
    lRSS :: !(UM.MVector s SplayIndex),
    -- | Decomposed node data storage: right children.
    rRSS :: !(UM.MVector s SplayIndex),
    -- | Decomposed node data storage: parents.
    pRSS :: !(UM.MVector s SplayIndex),
    -- | Decomposed node data storage: subtree sizes.
    sRSS :: !(UM.MVector s Int),
    -- | Decomposed node data storage: payloads.
    vRSS :: !(UM.MVector s v),
    -- | Decomposed node data storage: aggregation of payloads.
    aggRSS :: !(UM.MVector s v),
    -- | Decomposed node data storage: reversed flag of children.
    revRSS :: !(UM.MVector s Bit),
    -- | Decomposed node data storage: lazily propagated semigroup action.
    actRSS :: !(UM.MVector s a)
  }

-- | \(O(N)\) Creates a new `RawSplaySeq` of capacity @n@ with lazily propagated values.
{-# INLINE newRSS #-}
newRSS :: (U.Unbox v, U.Unbox a, PrimMonad m) => Int -> m (RawSplaySeq (PrimState m) v a)
newRSS n = do
  freeRSS <- newPool n
  lRSS <- UM.unsafeNew n
  rRSS <- UM.unsafeNew n
  pRSS <- UM.unsafeNew n
  sRSS <- UM.unsafeNew n
  vRSS <- UM.unsafeNew n
  aggRSS <- UM.unsafeNew n
  revRSS <- UM.unsafeNew n
  actRSS <- UM.unsafeNew n
  return $ RawSplaySeq {capacityRSS = n, ..}

-- | \(O(1)\) Returns the number of elements stored. Requires monad for tracking the state.
{-# INLINE sizeRSS #-}
sizeRSS :: (PrimMonad m) => RawSplaySeq (PrimState m) v a -> m Int
sizeRSS = sizePool . freeRSS

-- | \(O(1)\) Returns the number of elements in a sequence.
{-# INLINE seqSizeRSS #-}
seqSizeRSS :: (HasCallStack, PrimMonad m) => RawSplaySeq (PrimState m) v a -> SplayIndex -> m Int
seqSizeRSS seq root
  | nullSI root = return 0
  | otherwise = GM.read (sRSS seq) $ coerce root

-- * Allocation

-- | \(O(1)\) Allocates a new node as a single element sequence.
{-# INLINE allocNodeRSS #-}
allocNodeRSS :: (HasCallStack, PrimMonad m, U.Unbox v, Monoid a, U.Unbox a) => RawSplaySeq (PrimState m) v a -> v -> m SplayIndex
allocNodeRSS RawSplaySeq {..} !v = do
  i <- coerce <$> allocPool freeRSS ()
  GM.write lRSS i undefSI
  GM.write rRSS i undefSI
  GM.write pRSS i undefSI
  GM.write sRSS i (1 :: Int)
  GM.write vRSS i v
  GM.write aggRSS i v
  GM.write revRSS i $ Bit False
  GM.write actRSS i mempty
  return $ coerce i

-- | \(O(N)\) Allocates a new sequence, internally as a binary tree from the bottom to the top.
allocSeqRSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a) => RawSplaySeq (PrimState m) v a -> U.Vector v -> m SplayIndex
allocSeqRSS seq@RawSplaySeq {..} !xs = do
  -- [l, r)
  let inner l r
        | l >= r = return undefSI
        | l + 1 == r = allocNodeRSS seq $ xs G.! l
        | otherwise = do
            let !m = (l + r) `div` 2
            rootL <- inner l m
            root <- allocNodeRSS seq (xs G.! m)
            rootR <- inner (m + 1) r
            unless (nullSI rootL) $ do
              GM.write lRSS (coerce root) rootL
              GM.write pRSS (coerce rootL) root
            unless (nullSI rootR) $ do
              GM.write rRSS (coerce root) rootR
              GM.write pRSS (coerce rootR) root
            updateNodeRSS seq root
            return root
  inner 0 (G.length xs)

-- | \(O(1)\) Frees a node.
freeNodeRSS :: (PrimMonad m) => RawSplaySeq (PrimState m) v a -> SplayIndex -> m ()
freeNodeRSS = deallocPool . freeRSS

-- | \(O(N)\) Frees a subtree.
freeSubtreeRSS :: (HasCallStack, PrimMonad m) => RawSplaySeq (PrimState m) v a -> SplayIndex -> m ()
freeSubtreeRSS seq@RawSplaySeq {..} =
  fix $ \dfs i -> do
    -- FIXME: efficiently read part of the node
    -- TODO: optics?
    l <- GM.read lRSS $ coerce i
    r <- GM.read rRSS $ coerce i
    -- free children first
    unless (nullSI l) $ dfs l
    unless (nullSI r) $ dfs r
    freeNodeRSS seq i

-- | \(O(1)\) Asserts the node is the root.
{-# INLINE assertRootRSS #-}
assertRootRSS :: (HasCallStack, PrimMonad m) => RawSplaySeq (PrimState m) v a -> SplayIndex -> m ()
assertRootRSS RawSplaySeq {..} root = dbgM $ do
  let !_ = assert (not (nullSI root)) "null as a root"
  p <- GM.read pRSS $ coerce root
  let !_ = assert (nullSI p) "not a root"
  return ()

-- * API

--
-- These functions follow the state monad in their API for the root index.

-- | Amortized \(O(\log N)\). Reads a kth node's value.
{-# INLINE readRSS #-}
readRSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SegmentAction a v, Eq a) => RawSplaySeq (PrimState m) v a -> Int -> SplayIndex -> m (v, SplayIndex)
readRSS seq@RawSplaySeq {..} k root = do
  assertRootRSS seq root
  root' <- splayKthRSS seq root k
  (,root') <$> GM.read vRSS (coerce root')

-- TODO: freezeRSS (get_all)

-- | Amortized \(O(\log N)\). Writes a kth node's value.
{-# INLINE writeRSS #-}
writeRSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SegmentAction a v, Eq a) => RawSplaySeq (PrimState m) v a -> Int -> v -> SplayIndex -> m SplayIndex
writeRSS seq k v root = do
  assertRootRSS seq root
  root' <- splayKthRSS seq root k
  writeNodeRSS seq root' v
  return root'

-- | Amortized \(O(\log N)\). Modifies a kth node's value.
{-# INLINE modifyRSS #-}
modifyRSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SegmentAction a v, Eq a) => RawSplaySeq (PrimState m) v a -> (v -> v) -> Int -> SplayIndex -> m SplayIndex
modifyRSS seq f k root = do
  assertRootRSS seq root
  root' <- splayKthRSS seq root k
  modifyNodeRSS seq f root'
  return root'

-- | Amortized \(O(\log N)\). Exchanges a kth node's value.
{-# INLINE exchangeRSS #-}
exchangeRSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SegmentAction a v, Eq a) => RawSplaySeq (PrimState m) v a -> Int -> v -> SplayIndex -> m (v, SplayIndex)
exchangeRSS seq k v root = do
  assertRootRSS seq root
  root' <- splayKthRSS seq root k
  res <- exchangeNodeRSS seq root' v
  return (res, root')

-- | Amortized \(O(\log N)\). Folds an interval @[l, r]@.
{-# INLINE foldRSS #-}
foldRSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SegmentAction a v, Eq a) => RawSplaySeq (PrimState m) v a -> Int -> Int -> SplayIndex -> m (v, SplayIndex)
foldRSS seq@RawSplaySeq {..} l r root
  | l > r = return (mempty, root)
  | otherwise = do
      size <- GM.read sRSS $ coerce root
      -- TODO: foldMayRSS
      let !_ = assert (0 <= l && l <= r && r < size) "invalid interval"
      assertRootRSS seq root
      target <- captureRSS seq root l (r + 1)
      res <- GM.read aggRSS $ coerce target
      splayRSS seq target True
      return (res, target)

-- | Amortized \(O(\log N)\). Folds the whole sequence.
{-# INLINE foldAllRSS #-}
foldAllRSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SegmentAction a v, Eq a) => RawSplaySeq (PrimState m) v a -> SplayIndex -> m v
foldAllRSS seq@RawSplaySeq {..} root = do
  assertRootRSS seq root
  GM.read aggRSS $ coerce root

-- | Amortized \(O(\log N)\).
{-# INLINE sactRSS #-}
sactRSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SegmentAction a v, Eq a) => RawSplaySeq (PrimState m) v a -> Int -> Int -> a -> SplayIndex -> m SplayIndex
sactRSS seq@RawSplaySeq {..} l r act root = do
  -- TODO: assert with size
  let !_ = assert (0 <= l && l <= r && r < capacityRSS) $ "invalid interval: " ++ show (l, r)
  assertRootRSS seq root
  root' <- captureRSS seq root l (r + 1)
  sactNodeRSS seq root' act
  splayRSS seq root' True
  return root'

-- | Amortized \(O(\log N)\). Reverses the order of nodes in given range @[l, r]@. Requires the
-- monoid and the action to be commutative.
{-# INLINE reverseRSS #-}
reverseRSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SegmentAction a v, Eq a) => RawSplaySeq (PrimState m) v a -> Int -> Int -> SplayIndex -> m SplayIndex
reverseRSS seq l r root0
  | l > r = return root0
  | otherwise = do
      root' <- captureRSS seq root0 l (r + 1)
      reverseNodeRSS seq root'
      splayRSS seq root' True
      return root'

-- | Amortized \(O(\log N)\). Inserts a node at @k@.
{-# INLINE insertRSS #-}
insertRSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SegmentAction a v, Eq a) => RawSplaySeq (PrimState m) v a -> Int -> v -> SplayIndex -> m SplayIndex
insertRSS seq k v root = do
  (!l, !r) <- splitAtRSS seq root k
  node <- allocNodeRSS seq v
  merge3RSS seq l node r

-- | Amortized \(O(\log N)\). Deletes a node at @k@.
{-# INLINE deleteRSS #-}
deleteRSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SegmentAction a v, Eq a) => RawSplaySeq (PrimState m) v a -> Int -> SplayIndex -> m SplayIndex
deleteRSS seq i root = do
  (!l, !m, !r) <- split3RSS seq root i (i + 1)
  freeNodeRSS seq m
  root' <- mergeRSS seq l r
  return root'

-- | Amortized \(O(\log N)\). Bisection method over the sequence. Partition point. Note that The
-- user function is run over each node, not fold of an interval.
{-# INLINE bisectLRSS #-}
bisectLRSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SegmentAction a v, Eq a) => RawSplaySeq (PrimState m) v a -> (v -> Bool) -> SplayIndex -> m (Maybe SplayIndex, SplayIndex)
bisectLRSS seq@RawSplaySeq {..} check root0 = do
  let inner root parent lastYes
        | nullSI root && nullSI lastYes = return (Nothing, parent)
        | nullSI root = return (Just lastYes, parent)
        | otherwise = do
            propNodeRSS seq root
            v <- GM.read vRSS $ coerce root
            if check v
              then do
                r <- GM.read rRSS $ coerce root
                inner r root root
              else do
                l <- GM.read lRSS $ coerce root
                inner l root lastYes

  (!found, root') <- inner root0 undefSI undefSI
  splayRSS seq root' True
  return (found, root')

-- * Self-balancing methods (internals)

--
-- These functions take root indices as object, not context.

-- | Amortized \(O(\log N)\). Rotates the node. Propagation and updates are done outside of the
-- function (see `propFromRootRSS`).
rotateRSS :: (HasCallStack, PrimMonad m) => RawSplaySeq (PrimState m) v a -> SplayIndex -> m ()
rotateRSS RawSplaySeq {..} !i = do
  p <- GM.read pRSS $ coerce i
  pl <- GM.read lRSS $ coerce p

  c <-
    if pl == i
      then do
        --   p       i
        --  /         \
        -- i     ->    p
        --  \         /
        --   r       r
        r <- GM.exchange rRSS (coerce i) p
        GM.write lRSS (coerce p) r
        return r
      else do
        -- p          i
        --  \        /
        --   i  ->  p
        --  /        \
        -- l          l
        l <- GM.exchange lRSS (coerce i) p
        GM.write rRSS (coerce p) l
        return l

  pp <- GM.read pRSS $ coerce p
  unless (nullSI pp) $ do
    --   pp      pp
    --  /    -> /
    -- p       i
    GM.modify lRSS (\ppl -> if ppl == p then i else ppl) $ coerce pp
    --   pp       pp
    --     \  ->    \
    --      p        i
    GM.modify rRSS (\ppr -> if ppr == p then i else ppr) $ coerce pp

  -- set parents
  GM.write pRSS (coerce i) pp
  GM.write pRSS (coerce p) i
  unless (nullSI c) $ do
    GM.write pRSS (coerce c) p

-- | Amortized \(O(\log N)\). Moves a node up to the root, performing self-balancing heuristic
-- called rotations.
--
-- = Prerequisites
-- Parents are already propagated.
--
-- = After call
-- The node is updated and propagated.
splayRSS :: (HasCallStack, PrimMonad m, U.Unbox v, Monoid v, Monoid a, U.Unbox a, SegmentAction a v, Eq a) => RawSplaySeq (PrimState m) v a -> SplayIndex -> Bool -> m ()
splayRSS seq@RawSplaySeq {..} i doneParentProp = do
  if doneParentProp
    then propNodeRSS seq i
    else propNodeFromRootRSS seq i

  fix $ \loop -> do
    p <- GM.read pRSS $ coerce i
    unless (nullSI p) $ do
      pp <- GM.read pRSS $ coerce p
      if nullSI pp
        then do
          rotateRSS seq i
          updateNodeRSS seq p
          return ()
        else do
          pl <- GM.read lRSS $ coerce p
          pr <- GM.read rRSS $ coerce p
          ppl <- GM.read lRSS $ coerce pp
          ppr <- GM.read rRSS $ coerce pp
          if pl == i && ppl == p || pr == i && ppr == p
            then do
              -- same direction twice
              rotateRSS seq p
              rotateRSS seq i
            else do
              rotateRSS seq i
              rotateRSS seq i
          updateNodeRSS seq pp
          updateNodeRSS seq p
      loop

  updateNodeRSS seq i

-- | Amortized \(O(\log N)\). Finds @k@ th node from the left and splays it.
-- Returns the new root.
splayKthRSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SegmentAction a v, Eq a) => RawSplaySeq (PrimState m) v a -> SplayIndex -> Int -> m SplayIndex
splayKthRSS seq@RawSplaySeq {..} root0 k0 = do
  size <- GM.read sRSS $ coerce root0
  let !_ = assert (0 <= k0 && k0 < size) "no kth element in the sequence"

  let inner root k = do
        propNodeRSS seq root
        l <- GM.read lRSS $ coerce root
        -- The number of left children = the node's index counting from the leftmost.
        sizeL <- if nullSI l then return 0 else GM.read sRSS $ coerce l
        case compare k sizeL of
          EQ -> return root
          LT -> inner l k
          GT -> do
            r <- GM.read rRSS $ coerce root
            inner r (k - (sizeL + 1))

  target <- inner root0 k0
  splayRSS seq target True
  return target

-- * Node operations (internals)

-- | \(O(1)\) Recomputes the node size and the monoid aggregation.
{-# INLINE updateNodeRSS #-}
updateNodeRSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v) => RawSplaySeq (PrimState m) v a -> SplayIndex -> m ()
updateNodeRSS RawSplaySeq {..} i = do
  l <- GM.read lRSS $ coerce i
  r <- GM.read rRSS $ coerce i
  v <- GM.read vRSS $ coerce i
  -- FIXME: ignore mempty
  (!sizeL, !aggL) <-
    if nullSI l
      then return (0, mempty)
      else (,) <$> GM.read sRSS (coerce l) <*> GM.read aggRSS (coerce l)
  (!sizeR, !aggR) <-
    if nullSI r
      then return (0, mempty)
      else (,) <$> GM.read sRSS (coerce r) <*> GM.read aggRSS (coerce r)
  GM.write sRSS (coerce i) $! sizeL + 1 + sizeR
  GM.write aggRSS (coerce i) $! aggL <> v <> aggR

-- | \(O(1)\) Writes the monoid.
--
-- = Prerequisties
-- The node is a root (it's splayed).
{-# INLINE writeNodeRSS #-}
writeNodeRSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v) => RawSplaySeq (PrimState m) v a -> SplayIndex -> v -> m ()
writeNodeRSS seq@RawSplaySeq {..} root v = do
  assertRootRSS seq root
  GM.write vRSS (coerce root) v
  updateNodeRSS seq root

-- | \(O(1)\) Modifies the monoid.
--
-- = Prerequisties
-- The node is a root (it's splayed).
{-# INLINE modifyNodeRSS #-}
modifyNodeRSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v) => RawSplaySeq (PrimState m) v a -> (v -> v) -> SplayIndex -> m ()
modifyNodeRSS seq@RawSplaySeq {..} f root = do
  assertRootRSS seq root
  GM.modify vRSS f $ coerce root
  updateNodeRSS seq root

-- | \(O(1)\) Modifies the monoid.
--
-- = Prerequisties
-- The node is a root (it's splayed).
{-# INLINE exchangeNodeRSS #-}
exchangeNodeRSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v) => RawSplaySeq (PrimState m) v a -> SplayIndex -> v -> m v
exchangeNodeRSS seq@RawSplaySeq {..} root v = do
  assertRootRSS seq root
  res <- GM.exchange vRSS (coerce root) v
  updateNodeRSS seq root
  return res

-- | \(O(1)\) Swaps the left and the right children.
{-# INLINE swapLrNodeRSS #-}
swapLrNodeRSS :: (HasCallStack, PrimMonad m) => RawSplaySeq (PrimState m) v a -> SplayIndex -> m ()
swapLrNodeRSS RawSplaySeq {..} i = do
  l <- GM.read lRSS $ coerce i
  r <- GM.exchange rRSS (coerce i) l
  GM.write lRSS (coerce i) r

-- | \(O(1)\) Reverses the left and the right children, lazily and recursively.
{-# INLINE reverseNodeRSS #-}
reverseNodeRSS :: (HasCallStack, PrimMonad m) => RawSplaySeq (PrimState m) v a -> SplayIndex -> m ()
reverseNodeRSS seq@RawSplaySeq {..} i = do
  swapLrNodeRSS seq i
  -- lazily propagate new reverse or cancel:
  GM.modify revRSS (xor (Bit True)) $ coerce i

-- | Amortized \(O(\log N)\). Propgates the lazily propagated values on a node.
{-# INLINE propNodeRSS #-}
propNodeRSS :: (HasCallStack, PrimMonad m, U.Unbox v, Monoid a, U.Unbox a, SegmentAction a v, Eq a) => RawSplaySeq (PrimState m) v a -> SplayIndex -> m ()
propNodeRSS seq@RawSplaySeq {..} i = do
  -- action
  act <- GM.exchange actRSS (coerce i) mempty
  when (act /= mempty) $ do
    l <- GM.read lRSS $ coerce i
    unless (nullSI l) $ do
      sactNodeRSS seq l act
    r <- GM.read rRSS $ coerce i
    unless (nullSI r) $ do
      sactNodeRSS seq r act

  -- reverse
  whenM (unBit <$> GM.exchange revRSS (coerce i) (Bit False)) $ do
    l <- GM.read lRSS $ coerce i
    unless (nullSI l) $ do
      -- propagate new reverse or cancel:
      reverseNodeRSS seq l
    r <- GM.read rRSS $ coerce i
    unless (nullSI r) $ do
      -- propagate new reverse or cancel:
      reverseNodeRSS seq r

-- | Amortized \(O(\log N)\). Propagetes from the root to the given node.
{-# INLINE propNodeFromRootRSS #-}
propNodeFromRootRSS :: (HasCallStack, PrimMonad m, U.Unbox v, Monoid a, U.Unbox a, SegmentAction a v, Eq a) => RawSplaySeq (PrimState m) v a -> SplayIndex -> m ()
propNodeFromRootRSS seq@RawSplaySeq {..} i0 = inner i0
  where
    inner i = do
      p <- GM.read pRSS $ coerce i
      unless (nullSI p) $ do
        inner p
      propNodeRSS seq i

-- | Amortized \(O(\log N)\). Propgates at a node.
{-# INLINE sactNodeRSS #-}
sactNodeRSS :: (HasCallStack, PrimMonad m, U.Unbox v, Monoid a, U.Unbox a, SegmentAction a v) => RawSplaySeq (PrimState m) v a -> SplayIndex -> a -> m ()
sactNodeRSS RawSplaySeq {..} i act = do
  len <- GM.read sRSS $ coerce i
  GM.modify vRSS (segAct act) $ coerce i
  GM.modify aggRSS (segActWithLength len act) $ coerce i
  GM.modify actRSS (act <>) $ coerce i

-- * Split / merge

--
-- These functions take root indices as object, not context.

-- | Amortized \(O(\log N)\). Merges two nodes. It's implemented a reverse operation of `splitRSS`.
mergeRSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SegmentAction a v, Eq a) => RawSplaySeq (PrimState m) v a -> SplayIndex -> SplayIndex -> m SplayIndex
mergeRSS seq@RawSplaySeq {..} l r
  | nullSI l = return r
  | nullSI r = return l
  | otherwise = do
      assertRootRSS seq l
      assertRootRSS seq r
      -- find leftmost
      r' <- splayKthRSS seq r 0
      GM.write lRSS (coerce r') l
      GM.write pRSS (coerce l) r'
      updateNodeRSS seq r'
      return r'

-- | Amortized \(O(\log N)\). Folds three nodes from left to right.
merge3RSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SegmentAction a v, Eq a) => RawSplaySeq (PrimState m) v a -> SplayIndex -> SplayIndex -> SplayIndex -> m SplayIndex
merge3RSS seq l m r = do
  node <- mergeRSS seq l m
  mergeRSS seq node r

-- | Amortized \(O(\log N)\). Splits a sequneces into two nodes. It's implemented a reverse
-- operation of `splitRSS`.
splitAtRSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SegmentAction a v, Eq a) => RawSplaySeq (PrimState m) v a -> SplayIndex -> Int -> m (SplayIndex, SplayIndex)
splitAtRSS seq@RawSplaySeq {..} root k
  | k == 0 = return (undefSI, root)
  | otherwise = do
      assertRootRSS seq root
      size <- GM.read sRSS $ coerce root
      if k == size
        then return (root, undefSI)
        else do
          root' <- splayKthRSS seq root (k - 1)
          r <- GM.exchange rRSS (coerce root') undefSI
          GM.write pRSS (coerce r) undefSI
          updateNodeRSS seq root'
          return (root', r)

-- | Amortized \(O(\log N)\). Splits into three sequences from right to left.
split3RSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SegmentAction a v, Eq a) => RawSplaySeq (PrimState m) v a -> SplayIndex -> Int -> Int -> m (SplayIndex, SplayIndex, SplayIndex)
split3RSS seq root l r = do
  (!root', !nodeR) <- splitAtRSS seq root r
  (!nodeL, !nodeM) <- splitAtRSS seq root' l
  return (nodeL, nodeM, nodeR)

-- | Amortized \(O(\log N)\). Captures a subtree of [l, r). Be sure that it's half-open interval!
-- Splay the new root after call.
captureRSS :: (HasCallStack, PrimMonad m, Monoid v, U.Unbox v, Monoid a, U.Unbox a, SegmentAction a v, Eq a) => RawSplaySeq (PrimState m) v a -> SplayIndex -> Int -> Int -> m SplayIndex
captureRSS seq@RawSplaySeq {..} root l r
  | l == 0 = do
      size <- GM.read sRSS $ coerce root
      if r == size
        then return root
        else do
          root' <- splayKthRSS seq root r
          GM.read lRSS $ coerce root'
  | otherwise = do
      size <- GM.read sRSS $ coerce root
      if r == size
        then do
          root' <- splayKthRSS seq root (l - 1)
          GM.read rRSS $ coerce root'
        else do
          -- o--l--o--o--r--o
          --    [        )
          --             * root' (splayed)
          --          * rootL (detached from the root)
          -- \* rootL' (splayed)
          --    * right(rootL'): node that corresponds to [l, r)
          root' <- splayKthRSS seq root r
          rootL <- GM.read lRSS $ coerce root'
          -- detach `rootL` from `root'`
          GM.write pRSS (coerce rootL) undefSI
          rootL' <- splayKthRSS seq rootL (l - 1)
          -- re-attach `rootL'` to `root'`
          GM.write pRSS (coerce rootL') root'
          GM.write lRSS (coerce root') rootL'
          updateNodeRSS seq root'
          GM.read rRSS $ coerce rootL'
