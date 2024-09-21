{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

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
import Control.Monad (forM_, unless, when)
import Control.Monad.Extra (unlessM)
import Control.Monad.Fix (fix)
import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Control.Monad.Trans.State.Strict (execStateT, modify')
import Data.Bit
import Data.Bits
import Data.Bool (bool)
import Data.Graph.Alias
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Debug.Trace
import GHC.Stack (HasCallStack)
import ToyLib.Debug

-- | Strongly typed index of nodes in a `LCT`.
type IndexLCT = Vertex

{-# INLINE undefLCT #-}
undefLCT :: IndexLCT
undefLCT = -1

{-# INLINE nullLCT #-}
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
    -- TODO: subtree info??
    -- -- | Decomposed node data storage: FIXME: what?
    -- midLCT :: !(UM.MVector s a)
  }

-- | \(O(N)\)
newLCT :: (Monoid a, U.Unbox a, PrimMonad m) => Int -> m (LCT (PrimState m) a)
newLCT n = stToPrim $ do
  lLCT <- UM.replicate n undefLCT
  rLCT <- UM.replicate n undefLCT
  pLCT <- UM.replicate n undefLCT
  sLCT <- UM.replicate n 0
  revLCT <- UM.replicate n (Bit False)
  vLCT <- UM.replicate n mempty
  aggLCT <- UM.replicate n mempty
  -- midLCT <- UM.replicate n mempty
  return LCT {..}

-- | \(O(N + E \log E)\)
buildLCT :: (Monoid a, U.Unbox a, PrimMonad m) => U.Vector a -> U.Vector (Vertex, Vertex) -> m (LCT (PrimState m) a)
buildLCT xs es = stToPrim $ do
  lct <- do
    let !n = U.length xs
    lLCT <- UM.replicate n undefLCT
    rLCT <- UM.replicate n undefLCT
    pLCT <- UM.replicate n undefLCT
    sLCT <- UM.replicate n 0
    revLCT <- UM.replicate n (Bit False)
    vLCT <- U.thaw xs
    aggLCT <- UM.replicate n mempty
    -- midLCT <- UM.replicate n mempty
    return LCT {..}
  U.forM_  es $ \(!u, !v) -> do
    linkLCT lct u v
  return lct

-- TODO: build method

-- * Balancing

-- | \(O(1)\) Rotates up a non-root node.
rotateNodeLCT :: (HasCallStack, PrimMonad m, Monoid a, U.Unbox a) => LCT (PrimState m) a -> IndexLCT -> m ()
rotateNodeLCT lct@LCT {..} v = stToPrim $ do
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
            -- overwrite the light (path-parent) pointer:
            changeLightLCT lct p v

  -- update parent pointers to `pp`: pp <-- v <-- p <-- c
  GM.write pLCT v pp
  GM.write pLCT p v
  unless (nullLCT c) $ do
    GM.write pLCT c p

-- | Amortized \(O(\log N)\). Moves a node up to the root, performing self-balancing heuristic
-- called rotations.
splayLCT :: (HasCallStack, PrimMonad m, Monoid a, U.Unbox a) => LCT (PrimState m) a -> IndexLCT -> m ()
splayLCT lct@LCT {..} c = stToPrim $ do
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
          propNodeLCT lct pp
          propNodeLCT lct p
          propNodeLCT lct c
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

              rotateNodeLCT lct p
              rotateNodeLCT lct c
            else do
              --       pp         pp         c
              --      /          /          | \
              --    p     ->   c      ->   p   pp
              --     \        /
              --      c      p
              rotateNodeLCT lct c
              rotateNodeLCT lct c
      loop

-- * Node helpers

-- | \(O(1)\)
{-# INLINE isRootNodeLCT #-}
isRootNodeLCT :: (HasCallStack, PrimMonad m) => LCT (PrimState m) a -> IndexLCT -> m Bool
isRootNodeLCT lct v = do
  (== RootNodeLCT) <$> nodePlaceLCT lct v

-- TODO: return heavy/light notion
data NodePlaceLCT = RootNodeLCT | LeftNodeLCT | RightNodeLCT
  deriving (Eq)

-- | \(O(1)\)
nodePlaceLCT :: (HasCallStack, PrimMonad m) => LCT (PrimState m) a -> IndexLCT -> m NodePlaceLCT
nodePlaceLCT LCT {..} v = stToPrim $ do
  p <- GM.read pLCT v
  if nullLCT p
    then return RootNodeLCT
    else do
      pl <- GM.read lLCT p
      if pl == v
        then return LeftNodeLCT
        else do
          pr <- GM.read rLCT p
          return $ bool RootNodeLCT RightNodeLCT $ pr == v

-- * Node operations

-- | Amortized \(O(\log N)\). Propgates the lazily propagated values on a node.
propNodeLCT :: (HasCallStack, PrimMonad m) => LCT (PrimState m) a -> IndexLCT -> m ()
propNodeLCT lct@LCT {..} v = stToPrim $ do
  Bit b <- GM.exchange revLCT v (Bit False)
  when b $ do
    l <- GM.read lLCT v
    r <- GM.read rLCT v
    unless (nullLCT l) $ reverseNodeLCT lct l
    unless (nullLCT r) $ reverseNodeLCT lct r

-- | \(O(1)\)
{-# INLINE reverseNodeLCT #-}
reverseNodeLCT :: (HasCallStack, PrimMonad m) => LCT (PrimState m) a -> IndexLCT -> m ()
reverseNodeLCT lct@LCT {..} i = do
  swapLrNodeLCT lct i
  -- lazily propagate new reverse from the children, or cancel:
  GM.modify revLCT (xor (Bit True)) i

-- | \(O(1)\) Reverses the left and the right children, lazily and recursively.
{-# INLINE swapLrNodeLCT #-}
swapLrNodeLCT :: (HasCallStack, PrimMonad m) => LCT (PrimState m) a -> IndexLCT -> m ()
swapLrNodeLCT LCT {..} i = do
  l <- GM.read lLCT i
  r <- GM.exchange rLCT i l
  GM.write lLCT i r

-- | \(O(1)\) Recomputes the node size and the monoid aggregation.
updateNodeLCT :: (HasCallStack, PrimMonad m, Monoid a, U.Unbox a) => LCT (PrimState m) a -> IndexLCT -> m ()
updateNodeLCT LCT {..} i = stToPrim $ do
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

-- | \(O(1)\) Adds a path-parent edge.
addLightLCT :: (PrimMonad m) => LCT (PrimState m) a -> IndexLCT -> IndexLCT -> m ()
addLightLCT lct@LCT {..} u v = do
  return ()

-- | \(O(1)\) TODO: what?
changeLightLCT :: (PrimMonad m) => LCT (PrimState m) a -> IndexLCT -> IndexLCT -> m ()
changeLightLCT lct@LCT {..} u v = do
  return ()

-- | \(O(1)\) Removes a path-parent edge.
eraseLightLCT :: (PrimMonad m) => LCT (PrimState m) a -> IndexLCT -> IndexLCT -> m ()
eraseLightLCT lct@LCT {..} u v = do
  return ()

-- * Link/cut operations

-- FIXME: isn't it log^2 N?

-- | Amortized \(O(\log N)\). Makes the root of the underlying tree and @v0@ to be in the same
-- preferred path (auxiliary tree). @v0@ will be the root of the auxiliary tree. Right child of
-- @v0@ will be null.
exposeLCT :: (HasCallStack, PrimMonad m, Monoid a, U.Unbox a) => LCT (PrimState m) a -> IndexLCT -> m IndexLCT
exposeLCT lct@LCT {..} v0 = stToPrim $ do
  let inner v lastRoot
        | nullLCT v = return lastRoot
        | otherwise = do
            -- go up to the top of the auxiliary tree:
            splayLCT lct v

            -- make @lastRoot@ the right child of @v@:
            --    v               v
            --   /|\        ->   /|\
            --    | r             | lastRoot  <-- @v0@ (in the @lastRoot@) will be connected to the root
            --    lastRoot        r
            r <- GM.read rLCT v
            unless (nullLCT r) $ addLightLCT lct v r
            unless (nullLCT lastRoot) $ eraseLightLCT lct v lastRoot
            GM.write rLCT v lastRoot
            updateNodeLCT lct v

            -- go up to the next auxiliary tree:
            --    p
            --    |
            --    v
            --     \
            --      lastRoot
            vp <- GM.read pLCT v
            inner vp v

  res <- inner v0 undefLCT

  do
    -- FIXME: remove
    pRes <- GM.read pLCT res
    unless (nullLCT pRes) $ error $ "xxx must be null!!! " ++ show (res, pRes)

  splayLCT lct v0

  do
    -- FIXME: remove
    p <- GM.read pLCT v0
    unless (nullLCT p) $ error $ "must be null!!! " ++ show (res, v0, p)

  return res

-- | Amortized \(O(\logN)\). Makes the root and @v0@ to be in the same preferred path (same
-- auxiliary tree).
{-# INLINE exposeLCT_ #-}
exposeLCT_ :: (HasCallStack, PrimMonad m, Monoid a, U.Unbox a) => LCT (PrimState m) a -> IndexLCT -> m ()
exposeLCT_ lct v0 = do
  _ <- exposeLCT lct v0
  return ()

-- | Amortized \(O(\logN)\). Makes @v@ a new root of the underlying tree.
{-# INLINE evertLCT #-}
evertLCT :: (HasCallStack, PrimMonad m, Monoid a, U.Unbox a) => LCT (PrimState m) a -> IndexLCT -> m ()
evertLCT lct v = do
  -- make @v@ be in the same preferred path as root. note that @v@ is at the root of the auxiliary tree.
  exposeLCT_ lct v
  -- reverse all the edges with respect to @v@: make @v@ a new root of the underlying tree.
  reverseNodeLCT lct v
  propNodeLCT lct v

-- | \(O(\log N)\) Returns i-th vertex of a path between @u@, @v@.
jumpLCT :: (HasCallStack, PrimMonad m, Monoid a, U.Unbox a) => LCT (PrimState m) a -> IndexLCT -> IndexLCT -> Int -> m IndexLCT
jumpLCT lct@LCT {..} u0 v0 k0 = stToPrim $ do
  -- make @v0@ a new root of the underlying tree
  evertLCT lct v0
  -- make @u0@ in the same preferred path as the root
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
-- readLCT :: (HasCallStack, PrimMonad m, Monoid a, U.Unbox a) => LCT (PrimState m) a -> Vertex -> m a
-- readLCT lct v = do
--   GM.read (vLCT lct) v

-- | Amortized \(O(\log N)\).
{-# INLINE writeLCT #-}
writeLCT :: (HasCallStack, PrimMonad m, Monoid a, U.Unbox a) => LCT (PrimState m) a -> Vertex -> a -> m ()
writeLCT lct v x = do
  -- make @v@ the new root of the underlying tree:
  evertLCT lct v
  -- write to it. FIXME: update the aggregated value?
  GM.write (vLCT lct) v x

-- | Amortized \(O(\log N)\).
{-# INLINE modifyLCT #-}
modifyLCT :: (HasCallStack, PrimMonad m, Monoid a, U.Unbox a) => LCT (PrimState m) a -> (a -> a) -> Vertex -> m ()
modifyLCT lct f v = do
  -- make @v@ the new root of the underlying tree:
  evertLCT lct v
  -- write to it. FIXME: update the aggregated value?
  GM.modify (vLCT lct) f v

-- | Amortized \(O(\log N)\). Creates an edge between @(c, p)@. In the represented tree, parent of
-- @c@ is @p@ after the operation.
linkLCT :: (HasCallStack, PrimMonad m, Monoid a, U.Unbox a) => LCT (PrimState m) a -> Vertex -> Vertex -> m ()
linkLCT lct@LCT {..} c p = stToPrim $ do
  -- make @c@ the new root of the underlying tree
  evertLCT lct c
  -- remove right children of @p@.
  exposeLCT_ lct p
  propNodeLCT lct p

  dbgM $ do
    cp <- GM.read pLCT c
    let !_ = assert (nullLCT cp) $ "cp must be null: " ++ show (c, cp)
    pr <- GM.read rLCT p
    let !_ = assert (nullLCT pr) $ "pr must be null: " ++ show (p, pr)
    return ()

  do
    cp <- GM.read pLCT c
    let !_ = if nullLCT cp then () else error $ "cp must be null: " ++ show (c, cp)
    pr <- GM.read rLCT p
    let !_ = if nullLCT pr then () else error $ "pr must be null: " ++ show (p, pr)
    return ()

  -- connect with a heavy edge:
  GM.write pLCT c p
  GM.write rLCT p c
  updateNodeLCT lct p

-- | Amortized \(O(\log N)\). Deletes an edge between @(u, v)@.
cutLCT :: (HasCallStack, PrimMonad m, Monoid a, U.Unbox a) => LCT (PrimState m) a -> IndexLCT -> IndexLCT -> m ()
cutLCT lct@LCT {..} u v = stToPrim $ do
  -- make @u@ the new root of the underlying tree
  evertLCT lct u
  -- make @v@ in the same preferred path as the root
  exposeLCT_ lct v

  dbgM $ do
    -- @v@ does not have any right children. because @u@ and @v@ are neighbors, @vl@ is @u@.
    vp <- GM.read pLCT v
    let !_ = assert (nullLCT vp) "vp must be null"
    vl <- GM.read lLCT v
    let !_ = assert (vl == u) "vl must be `u`"
    return ()

  do
    -- @v@ does not have any right children. because @u@ and @v@ are neighbors, @vl@ is @u@.
    vp <- GM.read pLCT v
    vl <- GM.read lLCT v
    let !_ = if nullLCT vp then () else error "vp must be null"
    let !_ = if vl == u then () else error "vl must be `u`"
    return ()

  -- delete the heavy edge.
  -- vl <- GM.read lLCT v
  -- GM.write pLCT vl undefLCT
  GM.write pLCT u undefLCT
  GM.write lLCT v undefLCT
  updateNodeLCT lct v

-- | Amortized \(O(\log N)\). Folds a path between @(u, v)@.
{-# INLINE foldPathLCT #-}
foldPathLCT :: (HasCallStack, PrimMonad m, Monoid a, U.Unbox a) => LCT (PrimState m) a -> IndexLCT -> IndexLCT -> m a
foldPathLCT lct@LCT {..} u v = do
  -- make @u@ the root of the underlying tree
  evertLCT lct u
  -- make @v@ in the same preferred path as @u@
  exposeLCT_ lct v
  -- now that @v@ is at the root of the auxiliary tree, its aggregation value is the path folding:
  GM.read aggLCT v

-- | \(O(\log N)\)
foldSubtreeLCT :: (HasCallStack, PrimMonad m, Monoid a, U.Unbox a) => LCT (PrimState m) a -> IndexLCT -> IndexLCT -> m a
foldSubtreeLCT lct@LCT {..} v root = stToPrim $ do
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

-- | \(O(N)\) Collects the auxiliary tree vertices where @v0@ is contained.
collectHeavyPathLCT :: (HasCallStack, PrimMonad m) => LCT (PrimState m) a -> IndexLCT -> m [IndexLCT]
collectHeavyPathLCT lct@LCT {..} v0 = stToPrim $ do
  let goUp !v = do
        b <- isRootNodeLCT lct v
        if b
          then return v
          else do
            p <- GM.read pLCT v
            goUp p

  -- DFS from left to right that corrects the subtree vertices
  let dfs !v !rev = do
        vl <- GM.read lLCT v
        vr <- GM.read rLCT v
        Bit rev' <- GM.read revLCT v
        if not rev
          then do
            -- from left to right
            unless (nullLCT vl) $ dfs vl (rev `xor` rev')
            modify' (v :)
            unless (nullLCT vr) $ dfs vr (rev `xor` rev')
          else do
            -- from right to left
            unless (nullLCT vr) $ dfs vr (rev `xor` rev')
            modify' (v :)
            unless (nullLCT vl) $ dfs vl (rev `xor` rev')

  !root <- goUp v0
  (`execStateT` []) $ dfs root False

-- | \(O(N)\) Debug printing.
dbgLCT :: (HasCallStack, PrimMonad m) => LCT (PrimState m) a -> m ()
dbgLCT LCT {..} = {- dbgM $ -} stToPrim $ do
  let !_ = traceShow ("p", "l", "r", "rev", GM.length lLCT) ()
  forM_ [0 .. GM.length lLCT - 1] $ \i -> do
    p <- GM.read pLCT i
    l <- GM.read lLCT i
    r <- GM.read rLCT i
    rev <- GM.read revLCT i
    let !_ = trace ("- " ++ show (p, l, r, rev)) ()
    return ()
