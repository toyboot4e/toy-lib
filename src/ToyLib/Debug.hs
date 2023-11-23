-- | Debug utilities
module ToyLib.Debug where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.SegmentTree.Strict
import Data.UnionFind.Mutable
import qualified Data.Vector.Generic as G
import Data.Vector.IxVector
import ToyLib.Macro

-- | For use with `dbgS`
class ShowGrid a where
  showGrid :: a -> String
  showGridN :: Int -> a -> String

instance (G.Vector v a, Show a) => ShowGrid (IxVector (Int, Int) (v a)) where
  showGrid = showGridN 0
  showGridN !len !grid = unlines $ map f [y0 .. y1]
    where
      ((!y0, !x0), (!y1, !x1)) = boundsIV grid
      f !y = unwords $ map (showN . (grid @!) . (y,)) [x0 .. x1]
      showN x =
        let !s = show x
            !lenX = length s
         in replicate (len - lenX) ' ' ++ s

-- | `$` with `dbgId`
($$) :: (Show a) => (a -> b) -> a -> b
($$) lhs rhs = lhs (dbgId rhs)

infixr 0 $$

-- | `.` with `dbgId`. TODO: Working?
(.$) :: (Show b) => (b -> c) -> (a -> b) -> a -> c
g .$ f = \a -> let !b = dbgId (f a) in g b

infixr 9 .$

dbgGrid :: (ShowGrid a) => a -> ()
dbgGrid !gr = dbgS (showGrid gr)

dbgGridN :: (ShowGrid a) => Int -> a -> ()
dbgGridN !len !gr = dbgS (showGridN len gr)

dbgUF :: (PrimMonad m) => MUnionFind (PrimState m) -> m ()
dbgUF (MUnionFind vec) = dbgUM vec

dbgUM :: (Show (v a), G.Vector v a, PrimMonad m) => (G.Mutable v) (PrimState m) a -> m ()
dbgUM vec = do
  !xs' <- G.unsafeFreeze vec
  let !_ = dbg xs'
  return ()

dbgSTree :: (Show (v a), G.Vector v a, PrimMonad m) => SegmentTree (G.Mutable v) (PrimState m) a -> m ()
dbgSTree (SegmentTree _ mVec) = do
  !vec <- G.unsafeFreeze mVec
  -- REMARK: I'm using 0-based index and it has 2^n - 1 vertices
  let !_ = dbg (G.drop (G.length vec `div` 2 - 1) vec)
  return ()

dbgSTreeAll :: (Show (v a), G.Vector v a, PrimMonad m) => SegmentTree (G.Mutable v) (PrimState m) a -> m ()
dbgSTreeAll (SegmentTree _ mVec) = do
  !vec <- G.unsafeFreeze mVec
  flip fix (0 :: Int, 1 :: Int) $ \loop (!n, !len) -> do
    -- REMARK: I'm using 0-based index and it has 2^n - 1 vertices
    unless (G.length vec <= len) $ do
      let !vec' = G.take len . G.drop (len - 1) $ vec
      let !_ = dbgS $ "> " ++ show vec'
      loop (n + 1, 2 * len)
