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
import Data.Coerce
import Data.Core.SegmentAction
import Data.Graph.Alias
import Data.Maybe
import Data.Pool
import Data.SplaySeq.Raw
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Stack (HasCallStack)
import ToyLib.Debug

-- | Strongly typed index of nodes in a `LCT`.
type IndexLCT = PoolIndex

-- | Link/cut tree.
data LCT s = LCT
  { -- | Pool for free slot handing.
    freeLCT :: !(Pool s ()),
    -- | Decomposed node data storage: left children.
    lLCT :: !(UM.MVector s SplayIndex),
    -- | Decomposed node data storage: right children.
    rLCT :: !(UM.MVector s SplayIndex),
    -- | Decomposed node data storage: parents.
    pLCT :: !(UM.MVector s SplayIndex),
    -- | Decomposed node data storage: subtree sizes.
    sLCT :: !(UM.MVector s Int),
    -- | Decomposed node data storage: reverse flag.
    revLCT :: !(UM.MVector s Bit),
    -- -- | Decomposed node data storage: payloads.
    -- vRSS :: !(UM.MVector s v),
    -- -- | Decomposed node data storage: aggregation of payloads.
    -- aggRSS :: !(UM.MVector s v),
    -- | The root node.
    rootLCT :: !(UM.MVector s Int)
  }

-- * Balancing

-- | \(O(1)\) Rotates a non-root node.
rotateNodeLCT :: (PrimMonad m) => LCT (PrimState m) -> IndexLCT -> m ()
rotateNodeLCT lct@LCT {..} v = do
  p <- GM.read pLCT $ coerce v
  pp <- GM.read pLCT $ coerce p
  pl <- GM.read lLCT $ coerce p

  c <-
    if pl == v
      then do
        -- rotate right:
        --   p      v  <-- reference from `pp` is updated later
        --  /        \
        -- v    ->    p
        --  \        /
        --   c      c
        c <- GM.exchange rLCT (coerce v) $ coerce p
        GM.write lLCT (coerce p) c
        return c
      else do
        -- rotate left:
        -- p          v  <-- reference from `pp` is updated later
        --  \        /
        --   v  ->  p
        --  /        \
        -- c          c
        c <- GM.exchange lLCT (coerce v) p
        GM.write rLCT (coerce p) c
        return c

  updateNodeLCT lct p
  updateNodeLCT lct v

  -- update the reference from `pp`:
  unless (nullPI pp) $ do
    ppl <- GM.read lLCT (coerce pp)
    if ppl == p
      then GM.write lLCT (coerce pp) v
      else do
        ppr <- GM.read rLCT (coerce pp)
        if ppr == p
          then GM.write rLCT (coerce pp) v
          else do
            changeLightLCT lct p v

  -- update parent pointers to `pp`: pp <-- v <-- p <-- c
  GM.write pLCT (coerce v) pp
  GM.write pLCT (coerce p) v
  unless (nullPI c) $ do
    GM.write pLCT (coerce c) p

-- | Amortized \(O(\log N)\). The preferred path is changed to be the pathfrom the root to @v0@.
splayLCT :: (PrimMonad m) => LCT (PrimState m) -> IndexLCT -> m ()
splayLCT lct@LCT {..} v0 = do
  propNodeLCT lct v0
  let inner c = do
        unlessM (isRootNodeLCT lct c) $ do
          p <- GM.read pLCT (coerce c)
          pp <- if nullPI p then return undefPI else GM.read pLCT (coerce p)
          stateP <- nodePlaceLCT lct p
          stateC <- nodePlaceLCT lct c
          case stateP of
            RootNodeLCT -> do
              propNodeLCT lct p
              propNodeLCT lct c
              rotateNodeLCT lct c
            _ | stateP == stateC -> do
              --       pp       p         c
              --      /        / \         \
              --    p     ->  c   pp  ->    p
              --   /                         \
              -- c                            pp
              --
              -- Or:
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
            _ -> do
              --
              -- Or,
              --       pp       p           c
              --      /          \           \
              --    p     ->      pp  ->      p
              --     \           /             \
              --      c         c               pp
              propNodeLCT lct pp
              propNodeLCT lct p
              propNodeLCT lct c
              rotateNodeLCT lct c
              rotateNodeLCT lct c
  inner v0

-- * Node helpers

-- | \(O(1)\)
isRootNodeLCT :: (PrimMonad m) => LCT (PrimState m) -> IndexLCT -> m Bool
isRootNodeLCT lct@LCT {..} v = do
  (== undefPI) <$> GM.read pLCT (coerce v)

-- TODO: return heavy/light notion
data NodePlaceLCT = RootNodeLCT | LeftNodeLCT | RightNodeLCT

-- | \(O(1)\)
nodePlaceLCT :: (PrimMonad m) => LCT (PrimState m) -> IndexLCT -> m NodePlaceLCT
nodePlaceLCT lct@LCT {..} v = do
  p <- GM.read pLCT (coerce v)
  if nullPI p
    then return RootNodeLCT
    else do
      pl <- GM.read lLCT (coerce v)
      return . bool LeftNodeLCT RightNodeLCT $ nullPI pl

-- * Node operations

-- | Amortized \(O(\log N)\). Propgates the lazily propagated values on a node.
propNodeLCT :: (PrimMonad m) => LCT (PrimState m) -> IndexLCT -> m ()
propNodeLCT lct@LCT {..} v = do
  Bit b <- GM.exchange revLCT (coerce v) (Bit False)
  when b $ do
    l <- GM.read lLCT (coerce v)
    r <- GM.read rLCT (coerce v)
    unless (nullPI l) $ reverseNodeLCT lct l
    unless (nullPI r) $ reverseNodeLCT lct r

-- | \(O(1)\)
reverseNodeLCT :: (HasCallStack, PrimMonad m) => LCT (PrimState m) -> IndexLCT -> m ()
reverseNodeLCT lct@LCT {..} i = do
  swapLrNodeLCT lct i
  -- lazily propagate new reverse or cancel:
  GM.modify revLCT (xor (Bit True)) $ coerce i

-- | \(O(1)\) Reverses the left and the right children, lazily and recursively.
swapLrNodeLCT :: (HasCallStack, PrimMonad m) => LCT (PrimState m) -> IndexLCT -> m ()
swapLrNodeLCT LCT {..} i = do
  l <- GM.read lLCT $ coerce i
  r <- GM.exchange rLCT (coerce i) l
  GM.write lLCT (coerce i) r

-- | \(O(1)\) Recomputes the node size and the monoid aggregation.
updateNodeLCT :: (HasCallStack, PrimMonad m) => LCT (PrimState m) -> SplayIndex -> m ()
updateNodeLCT LCT {..} i = do
  sizeL <- fmap (fromMaybe 0) $ GM.readMaybe sLCT . coerce =<< GM.read lLCT (coerce i)
  sizeR <- fmap (fromMaybe 0) $ GM.readMaybe sLCT . coerce =<< GM.read rLCT (coerce i)
  GM.write sLCT (coerce i) $! sizeL + 1 + sizeR

addLightLCT :: (PrimMonad m) => LCT (PrimState m) -> IndexLCT -> IndexLCT -> m ()
addLightLCT lct@LCT {..} u v = do
  return ()

changeLightLCT :: (PrimMonad m) => LCT (PrimState m) -> IndexLCT -> IndexLCT -> m ()
changeLightLCT lct@LCT {..} u v = do
  return ()

eraseLightLCT :: (PrimMonad m) => LCT (PrimState m) -> IndexLCT -> IndexLCT -> m ()
eraseLightLCT lct@LCT {..} u v = do
  return ()

-- * Link/cut operations

-- | Amortized \(O(\logN)\). FIXME: What is it?
exposeLCT :: (PrimMonad m) => LCT (PrimState m) -> IndexLCT -> m IndexLCT
exposeLCT lct@LCT {..} v0 = do
  let inner v rp
        | nullPI v = return rp
        | otherwise = do
            splayLCT lct v
            r <- GM.read rLCT $ coerce v
            -- FIXME: what is happening?
            -- v
            --  \
            --   r
            unless (nullPI r) $ addLightLCT lct v r
            unless (nullPI rp) $ eraseLightLCT lct v rp
            GM.write rLCT (coerce v) rp
            updateNodeLCT lct v
            vp <- GM.read pLCT $ coerce v
            inner vp v

  res <- inner v0 undefPI
  splayLCT lct v0
  return res

-- | Amortized \(O(\logN)\). FIXME: What is it?
exposeLCT_ :: (PrimMonad m) => LCT (PrimState m) -> IndexLCT -> m ()
exposeLCT_ lct v0 = do
  _ <- exposeLCT lct v0
  return ()

-- | Amortized \(O(\logN)\). FIXME: What is it?
evertLCT :: (PrimMonad m) => LCT (PrimState m) -> IndexLCT -> m ()
evertLCT lct@LCT {..} v = do
  exposeLCT lct v
  reverseNodeLCT lct v
  propNodeLCT lct v

-- | \(O(\log N)\) Returns i-th vertex of a path between @u@, @v@.
jumpLCT :: (PrimMonad m) => LCT (PrimState m) -> IndexLCT -> IndexLCT -> Int -> m IndexLCT
jumpLCT lct@LCT {..} u0 v0 k0 = do
  -- FIXME: how does it make sure the two are in the same auxiliary tree (in the same path)
  evertLCT lct v0
  exposeLCT_ lct u0
  do
    size <- GM.read sLCT (coerce u0)
    let !_ = assert (0 <= k0 && k0 < size) "invalid jump"
    return ()

  -- FIXME: details
  let inner k u = do
        propNodeLCT lct u
        ur <- GM.read rLCT (coerce u)
        urSize <- if nullPI ur then return 0 else GM.read sLCT (coerce ur)
        if k < urSize
          then inner k ur
          else
            if k == urSize
              then return u
              else do
                ul <- GM.read lLCT (coerce u)
                inner (k - (urSize + 1)) ul

  res <- inner k0 u0
  splayLCT lct u0
  return res

-- * API

-- | Amortized \(O(\log N)\). Merges two
linkLCT :: (PrimMonad m) => LCT (PrimState m) -> IndexLCT -> IndexLCT -> m ()
linkLCT lct c p = d
  evertLCT lct c
  exposeLCT_ lct p
  propNodeLCT lct p
  dbgM $ do
    cp <- GM.read pLCT (coerce c)
    let !_ = assert (nullSI cp) "cp"
    pr <- GM.read pLCT (coerce p)
    let !_ = assert (nullSI pr) "pr"
    return ()
  GM.write pLCT (coerce c) p
  GM.write rLCT (coerce p) c
  updateNodeLCT lct p

-- | Amortized \(O(\log N)\). Merges two
cutLCT :: (PrimMonad m) => LCT (PrimState m) -> IndexLCT -> IndexLCT -> m ()
cutLCT lct u v = do
  evertLCT lct u
  evertLCT lct v
  GM.read vLCT lct v
  vl <- GM.read lLCT (coerce v)
  dbgM $ do
    vp <- GM.read pLCT (coerce v)
    let !_ = assert (nullSI vp) "vp"
    let !_ = assert (nullSI vl) "vl"
    return ()
  GM.write pLCT (coerce vl) undefSI
  GM.write lLCT (coerce v) undefSI
  updateNodeLCT lct v

-- | \(O(\log N)\)
foldPathLCT :: (PrimMonad m) => LCT (PrimState m) -> IndexLCT -> IndexLCT -> m a
foldPathLCT lct u v = do
cutLCT lct u v = do
  evertLCT lct u
  evertLCT lct v
  -- FIXME: what is x, rx and vx?
  -- GM.read aggLCT lct (coerce v)
  return _

-- | \(O(\log N)\)
foldSubtreeLCT :: (PrimMonad m) => LCT (PrimState m) -> IndexLCT -> m a
foldSubtreeLCT LCT {..} v root = do
  if v == root
    then do
      evertLCT lct root
      GM.read vLCT lct v
    else do
      root <- jumpLCT lct v root 1
      cutLCT lct v root
      ers <- GM.read vLCT (coerce v)
      linkLCT lct v root

-- | \(O(N)\) Collects heavy vertices in a subtree.
collectHeavyPathLCT :: (PrimMonad m) => LCT (PrimState m) -> IndexLCT -> m [IndexLCT]
collectHeavyPathLCT LCT {..} v0 = do
  let goUp !acc !v = do
        p <- GM.read pLCT (coerce v)
        if nullPI p
          then return (v : acc)
          else goUp (v : acc) p
  xs <- goUp [] v0

  -- DFS from left to right that corrects the subtree vertices
  let dfs !c !rev = do
        cl <- GM.read lLCT (coerce c)
        cr <- GM.read rLCT (coerce c)
        Bit rev' <- GM.read revLCT (coerce c)
        if not rev
          then do
            unless (nullPI cl) $ dfs cl (rev `xor` rev')
            modify' (c :)
            unless (nullPI cr) $ dfs cl (rev `xor` rev')
          else do
            unless (nullPI cr) $ dfs cl (rev `xor` rev')
            modify' (c :)
            unless (nullPI cl) $ dfs cl (rev `xor` rev')

  (`execStateT` xs) $ dfs v0 False
