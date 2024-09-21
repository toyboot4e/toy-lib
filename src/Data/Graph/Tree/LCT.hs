{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Link/cut tree.
--
-- - <https://en.wikipedia.org/wiki/Link/cut_tree>
-- - <https://www.cs.cmu.edu/~sleator/papers/self-adjusting.pdf>
--
-- = Structure
--
-- Link/cut tree splits a tree into paths. Each path is represented by an auxiliary tree, i.e.,
-- splay tree in our case. Nodes in an auxliary tree are sorted by their depth in the original tree.
-- Therefore the leftmost child has the smallest depth and considered to be the "root".
--
-- ODO: Since the auxiliary trees are keyed by depth, the root R will be the leftmost node of the auxiliary tree.
--
-- TODO: what are heavy/light edges
-- Paths are connected by "Path-parent pointer", i.e., a directed pointer from one tree root to a
-- parent tree node.
--
-- - Preferred path in the original tree: the heavy paths.
-- - Preferred path in an auxiliary tree:
--
-- @
--     p
--   / |\
--  l  | r
--     m
-- @
--
-- = Example
--
-- @
--  0--8--7--3--1--2--12--13--15--14     XX: vertex
--     |        |                         --: edge
-- 10--5        11--8--6                   |: path-parent edge
--     |
--     4
-- @
--
-- = Thanks
-- - [@link_cut_tree.hpp@ by maspypy/library](https://github.com/maspypy/library/blob/ad0ef89aac7e858dbf4cd2b587ec94659259f4b6/graph/ds/link_cut_tree.hpp#L8)
module Data.Graph.Tree.LCT where

import Control.Exception (assert)
import Control.Monad (unless, void, when)
import Control.Monad.Extra (unlessM, whenM)
import Control.Monad.Fix (fix)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.Trans.State.Strict (execStateT, modify')
import Data.Bit
import Data.Bits
import Data.Bool (bool)
import Data.Buffer
import Data.Core.SegmentAction
import Data.Graph.Alias
import Data.Maybe
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Stack (HasCallStack)
import ToyLib.Debug

-- | Strongly typed index of nodes in a `LCT`.
type IndexLCT = Vertex

undefLCT :: IndexLCT
undefLCT = -1

nullLCT :: IndexLCT -> Bool
nullLCT = (== -1)

-- | Link/cut tree.
data LCT s a = LCT
  { -- | Decomposed node data storage: left children.
    lLCT :: !(UM.MVector s IndexLCT),
    -- | Decomposed node data storage: right children.
    rLCT :: !(UM.MVector s IndexLCT),
    -- | Decomposed node data storage: parents.
    pLCT :: !(UM.MVector s IndexLCT),
    -- | Decomposed node data storage: subtree sizes.
    sLCT :: !(UM.MVector s Int),
    -- | Decomposed node data storage: reverse flag.
    revLCT :: !(UM.MVector s Bit),
    -- | Decomposed node data storage: payloads.
    vLCT :: !(UM.MVector s a),
    -- | Decomposed node data storage: aggregation of payloads.
    aggLCT :: !(UM.MVector s a)
  }

-- | \(O(N)\)
newLCT :: (PrimMonad m, Monoid a, U.Unbox a) => Int -> m (LCT (PrimState m) a)
newLCT n = do
  lLCT <- UM.replicate n undefLCT
  rLCT <- UM.replicate n undefLCT
  pLCT <- UM.replicate n undefLCT
  sLCT <- UM.replicate n 0
  revLCT <- UM.replicate n (Bit False)
  vLCT <- UM.replicate n mempty
  aggLCT <- UM.replicate n mempty
  return LCT {..}

-- * Balancing

-- | \(O(1)\) Rotates up a non-root node.
rotateNodeLCT :: (PrimMonad m, Monoid a, U.Unbox a) => LCT (PrimState m) a -> IndexLCT -> m ()
rotateNodeLCT lct@LCT {..} v = do
  p <- GM.read pLCT v
  pp <- GM.read pLCT p
  pl <- GM.read lLCT p

  c <-
    if pl == v
      then do
        -- rotate right:
        --   p      v  <-- reference from `pp` is updated later
        --  /        \
        -- v    ->    p
        --  \        /
        --   c      c
        c <- GM.exchange rLCT v p
        GM.write lLCT p c
        return c
      else do
        -- rotate left:
        -- p          v  <-- reference from `pp` is updated later
        --  \        /
        --   v  ->  p
        --  /        \
        -- c          c
        c <- GM.exchange lLCT v p
        GM.write rLCT p c
        return c

  updateNodeLCT lct p
  updateNodeLCT lct v

  -- update the reference from `pp`:
  unless (nullLCT pp) $ do
    ppl <- GM.read lLCT pp
    if ppl == p
      then GM.write lLCT pp v
      else do
        ppr <- GM.read rLCT pp
        if ppr == p
          then GM.write rLCT pp v
          else do
            changeLightLCT lct p v

  -- update parent pointers to `pp`: pp <-- v <-- p <-- c
  GM.write pLCT v pp
  GM.write pLCT p v
  unless (nullLCT c) $ do
    GM.write pLCT c p

-- | Amortized \(O(\log N)\). Moves a node up to the root, performing self-balancing heuristic
-- called rotations.
splayLCT :: (PrimMonad m, Monoid a, U.Unbox a) => LCT (PrimState m) a -> IndexLCT -> m ()
splayLCT lct@LCT {..} c = do
  propNodeLCT lct c
  fix $ \loop -> do
    unlessM (isRootNodeLCT lct c) $ do
      p <- GM.read pLCT c
      pp <- if nullLCT p then return undefLCT else GM.read pLCT p
      placeP <- nodePlaceLCT lct p
      if placeP == RootNodeLCT
        then do
          propNodeLCT lct p
          propNodeLCT lct c
          rotateNodeLCT lct c
        else do
          placeC <- nodePlaceLCT lct c
          if placeP == placeC
            then do
              -- Rotate right twice:
              --
              --       pp       p         c
              --      /        / \         \
              --    p     ->  c   pp  ->    p
              --   /                         \
              -- c                            pp

              -- Or rotate left twice:
              --
              --  pp             p            c
              --   \            / \          /
              --    p     ->  pp   c  ->    p
              --     \                     /
              --      c                   pp
              propNodeLCT lct pp
              propNodeLCT lct p
              propNodeLCT lct c
              rotateNodeLCT lct p
              rotateNodeLCT lct c
            else do
              --       pp       p           c
              --      /        / \           \
              --    p     -> c    pp  ->      p
              --     \                         \
              --      c                         pp

              propNodeLCT lct pp
              propNodeLCT lct p
              propNodeLCT lct c
              rotateNodeLCT lct c
              rotateNodeLCT lct c
      loop

-- * Node helpers

-- | \(O(1)\)
isRootNodeLCT :: (PrimMonad m) => LCT (PrimState m) a -> IndexLCT -> m Bool
isRootNodeLCT LCT {..} v = do
  (== undefLCT) <$> GM.read pLCT v

-- TODO: return heavy/light notion
data NodePlaceLCT = RootNodeLCT | LeftNodeLCT | RightNodeLCT
  deriving (Eq)

-- | \(O(1)\)
nodePlaceLCT :: (PrimMonad m) => LCT (PrimState m) a -> IndexLCT -> m NodePlaceLCT
nodePlaceLCT LCT {..} v = do
  p <- GM.read pLCT v
  if nullLCT p
    then return RootNodeLCT
    else do
      pl <- GM.read lLCT v
      return . bool LeftNodeLCT RightNodeLCT $ nullLCT pl

-- * Node operations

-- | Amortized \(O(\log N)\). Propgates the lazily propagated values on a node.
propNodeLCT :: (PrimMonad m) => LCT (PrimState m) a -> IndexLCT -> m ()
propNodeLCT lct@LCT {..} v = do
  Bit b <- GM.exchange revLCT v (Bit False)
  when b $ do
    l <- GM.read lLCT v
    r <- GM.read rLCT v
    unless (nullLCT l) $ reverseNodeLCT lct l
    unless (nullLCT r) $ reverseNodeLCT lct r

-- | \(O(1)\)
reverseNodeLCT :: (HasCallStack, PrimMonad m) => LCT (PrimState m) a -> IndexLCT -> m ()
reverseNodeLCT lct@LCT {..} i = do
  swapLrNodeLCT lct i
  -- lazily propagate new reverse or cancel:
  GM.modify revLCT (xor (Bit True)) i

-- | \(O(1)\) Reverses the left and the right children, lazily and recursively.
swapLrNodeLCT :: (HasCallStack, PrimMonad m) => LCT (PrimState m) a -> IndexLCT -> m ()
swapLrNodeLCT LCT {..} i = do
  l <- GM.read lLCT i
  r <- GM.exchange rLCT i l
  GM.write lLCT i r

-- | \(O(1)\) Recomputes the node size and the monoid aggregation.
updateNodeLCT :: (HasCallStack, PrimMonad m, Monoid a, U.Unbox a) => LCT (PrimState m) a -> IndexLCT -> m ()
updateNodeLCT LCT {..} i = do
  l <- GM.read lLCT i
  r <- GM.read rLCT i
  v <- GM.read vLCT i
  (!sizeL, !aggL) <-
    if nullLCT l
      then return (0, mempty)
      else (,) <$> GM.read sLCT l <*> GM.read aggLCT l
  (!sizeR, !aggR) <-
    if nullLCT r
      then return (0, mempty)
      else (,) <$> GM.read sLCT r <*> GM.read aggLCT r
  GM.write sLCT i $! sizeL + 1 + sizeR
  GM.write aggLCT i $! aggL <> v <> aggR

addLightLCT :: (PrimMonad m) => LCT (PrimState m) a -> IndexLCT -> IndexLCT -> m ()
addLightLCT lct@LCT {..} u v = do
  return ()

changeLightLCT :: (PrimMonad m) => LCT (PrimState m) a -> IndexLCT -> IndexLCT -> m ()
changeLightLCT lct@LCT {..} u v = do
  return ()

eraseLightLCT :: (PrimMonad m) => LCT (PrimState m) a -> IndexLCT -> IndexLCT -> m ()
eraseLightLCT lct@LCT {..} u v = do
  return ()

-- * Link/cut operations

-- | Amortized \(O(\logN)\). Make the root and @v0@ to be in the same preferred path (same
-- auxiliary tree).
exposeLCT :: (PrimMonad m, Monoid a, U.Unbox a) => LCT (PrimState m) a -> IndexLCT -> m IndexLCT
exposeLCT lct@LCT {..} v0 = do
  let inner v rp
        | nullLCT v = return rp
        | otherwise = do
            splayLCT lct v
            r <- GM.read rLCT v
            -- FIXME: what is happening?
            -- v
            --  \
            --   r
            unless (nullLCT r) $ addLightLCT lct v r
            unless (nullLCT rp) $ eraseLightLCT lct v rp
            GM.write rLCT v rp
            updateNodeLCT lct v
            vp <- GM.read pLCT v
            inner vp v

  res <- inner v0 undefLCT
  splayLCT lct v0
  return res

-- | Amortized \(O(\logN)\).
exposeLCT_ :: (PrimMonad m, Monoid a, U.Unbox a) => LCT (PrimState m) a -> IndexLCT -> m ()
exposeLCT_ lct v0 = do
  _ <- exposeLCT lct v0
  return ()

-- | Amortized \(O(\logN)\). Makes @v@ the root of the underlying tree.
evertLCT :: (PrimMonad m, Monoid a, U.Unbox a) => LCT (PrimState m) a -> IndexLCT -> m ()
evertLCT lct v = do
  exposeLCT_ lct v
  reverseNodeLCT lct v
  propNodeLCT lct v

-- | \(O(\log N)\) Returns i-th vertex of a path between @u@, @v@.
jumpLCT :: (PrimMonad m, Monoid a, U.Unbox a) => LCT (PrimState m) a -> IndexLCT -> IndexLCT -> Int -> m IndexLCT
jumpLCT lct@LCT {..} u0 v0 k0 = do
  -- make @v0@ a new root of the underlying
  evertLCT lct v0
  -- make @u0@ in the same preferred path as @v0@
  exposeLCT_ lct u0
  do
    size <- GM.read sLCT u0
    let !_ = assert (0 <= k0 && k0 < size) "invalid jump"
    return ()

  let inner k u = do
        propNodeLCT lct u
        -- FIXME: details
        ur <- GM.read rLCT u
        urSize <- if nullLCT ur then return 0 else GM.read sLCT ur
        if k < urSize
          then inner k ur
          else
            if k == urSize
              then return u
              else do
                ul <- GM.read lLCT u
                inner (k - (urSize + 1)) ul

  res <- inner k0 u0
  splayLCT lct u0
  return res

-- * API

-- -- | \(O(1)\)
-- readLCT :: (PrimMonad m, U.Unbox a) => LCT (PrimState m) a -> Vertex -> m a
-- readLCT lct v = do
--   GM.read (vLCT lct) v

-- | \(O(1)\)
writeLCT :: (PrimMonad m, U.Unbox a) => LCT (PrimState m) a -> Vertex -> a -> m ()
writeLCT lct v x = do
  GM.write (vLCT lct) v x

-- TODO: not update the aggregated value?

-- | \(O(1)\)
modifyLCT :: (PrimMonad m, U.Unbox a) => LCT (PrimState m) a -> (a -> a) -> Vertex -> m ()
modifyLCT lct f v = do
  GM.modify (vLCT lct) f v

-- | Amortized \(O(\log N)\). Merges two
linkLCT :: (PrimMonad m, Monoid a, U.Unbox a) => LCT (PrimState m) a -> Vertex -> Vertex -> m ()
linkLCT lct@LCT {..} c p = do
  evertLCT lct c
  exposeLCT_ lct p
  propNodeLCT lct p
  dbgM $ do
    cp <- GM.read pLCT c
    let !_ = assert (nullLCT cp) "cp"
    pr <- GM.read pLCT p
    let !_ = assert (nullLCT pr) "pr"
    return ()
  GM.write pLCT c p
  GM.write rLCT p c
  updateNodeLCT lct p

-- | Amortized \(O(\log N)\). Detaches
cutLCT :: (PrimMonad m, Monoid a, U.Unbox a) => LCT (PrimState m) a -> IndexLCT -> IndexLCT -> m ()
cutLCT lct@LCT {..} u v = do
  evertLCT lct u
  evertLCT lct v
  vl <- GM.read lLCT v
  dbgM $ do
    vp <- GM.read pLCT v
    let !_ = assert (nullLCT vp) "vp"
    let !_ = assert (nullLCT vl) "vl"
    return ()
  GM.write pLCT vl undefLCT
  GM.write lLCT v undefLCT
  updateNodeLCT lct v

-- | \(O(\log N)\)
foldPathLCT :: (PrimMonad m, Monoid a, U.Unbox a) => LCT (PrimState m) a -> IndexLCT -> IndexLCT -> m a
foldPathLCT lct@LCT {..} u v = do
  -- make @u@ the root of the underlying tree
  evertLCT lct u
  -- put @v@ in the same preferred path as @u@
  exposeLCT_ lct v
  -- FIXME: what is x, rx and vx?
  GM.read aggLCT v

-- | \(O(\log N)\)
foldSubtreeLCT :: (PrimMonad m, Monoid a, U.Unbox a) => LCT (PrimState m) a -> IndexLCT -> IndexLCT -> m a
foldSubtreeLCT lct@LCT {..} v root = do
  if v == root
    then do
      evertLCT lct root
      GM.read aggLCT v
    else do
      root' <- jumpLCT lct v root 1
      cutLCT lct v root'
      res <- GM.read aggLCT v
      linkLCT lct v root'
      return res

-- | \(O(N)\) Collects heavy vertices in a subtree.
collectHeavyPathLCT :: (PrimMonad m) => LCT (PrimState m) a -> IndexLCT -> m [IndexLCT]
collectHeavyPathLCT LCT {..} v0 = do
  let goUp !acc !v = do
        p <- GM.read pLCT v
        if nullLCT p
          then return (v : acc)
          else goUp (v : acc) p
  xs <- goUp [] v0

  -- DFS from left to right that corrects the subtree vertices
  let dfs !c !rev = do
        cl <- GM.read lLCT c
        cr <- GM.read rLCT c
        Bit rev' <- GM.read revLCT c
        if not rev
          then do
            unless (nullLCT cl) $ dfs cl (rev `xor` rev')
            modify' (c :)
            unless (nullLCT cr) $ dfs cl (rev `xor` rev')
          else do
            unless (nullLCT cr) $ dfs cl (rev `xor` rev')
            modify' (c :)
            unless (nullLCT cl) $ dfs cl (rev `xor` rev')

  (`execStateT` xs) $ dfs v0 False
