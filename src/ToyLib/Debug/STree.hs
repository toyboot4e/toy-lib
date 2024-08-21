-- | Debug utilities
module ToyLib.Debug.STree where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Core.SegmentAction
import Data.SegmentTree.Lazy
import Data.SegmentTree.Strict
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import ToyLib.Debug
import ToyLib.Macro

-- TODO: consider showing `mempty` values as `-`

-- | Shows the leaves of a strict segment tree.
--
-- WARNING: It shows unused leaves, too.
dbgSTree :: (Show a, U.Unbox a, PrimMonad m) => SegmentTree (PrimState m) a -> m ()
dbgSTree (SegmentTree mVec nValidLeaves)
  | debug = do
      !vec <- G.unsafeFreeze mVec
      -- REMARK: I'm using 0-based index and it has 2^n - 1 vertices
      -- TODO: drop non used slots?
      let !leaves = G.take nValidLeaves $ G.drop (G.length vec `div` 2) vec
      let !_ = dbg leaves
      return ()
  | otherwise = return ()

-- | Shows the nodes and the leaves of a strict segment tree,
--
-- WARNING: It shows unused nodes and leaves, too.
dbgSTreeAll :: (Show a, U.Unbox a, PrimMonad m) => SegmentTree (PrimState m) a -> m ()
dbgSTreeAll (SegmentTree mVec _)
  | debug = do
      !vec <- G.unsafeFreeze mVec
      flip fix (0 :: Int, 1 :: Int) $ \loop (!n, !len) -> do
        -- REMARK: I'm using 0-based index and it has 2^n - 1 vertices
        unless (G.length vec <= len) $ do
          let !vec' = G.take len . G.drop (len - 1) $ vec
          let !_ = dbgS $ "> " ++ show vec'
          loop (n + 1, 2 * len)
  | otherwise = return ()

-- | Shows the leaves of a lazily propagated segment tree.
dbgLSTree :: (Show a, Monoid a, U.Unbox a, Monoid op, SegmentAction op a, Eq op, U.Unbox op, PrimMonad m) => LazySegmentTree a op (PrimState m) -> m ()
dbgLSTree stree@(LazySegmentTree !vec _ _) = dbgSM $ do
  let !nLeaves = GM.length vec `div` 2
  ss <- forM [0 .. nLeaves - 1] $ \i -> do
    !x <- readLSTree stree i
    return $ show x
  return $ unwords ss
