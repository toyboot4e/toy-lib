-- | Debug utilities
module ToyLib.Debug where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.SegmentTree.Strict
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Data.Vector.IxVector
import ToyLib.Macro

-- | For use with `dbgS`
class ShowGrid a where
  showGrid :: a -> String

instance (G.Vector v a, Show a) => ShowGrid (IxVector (Int, Int) (v a)) where
  showGrid !grid = unlines $ map f [y0 .. y1]
    where
      ((!y0, !x0), (!y1, !x1)) = boundsIV grid
      f !y = unwords $ map (show . (grid @!) . (y,)) [x0 .. x1]

-- | `$` with `dbgId`
($$) :: (Show a) => (a -> b) -> a -> b
($$) lhs rhs = lhs (dbgId rhs)

infixr 0 $$

-- | `.` with `dbgId`
(.$) :: (Show b) => (b -> c) -> (a -> b) -> a -> c
g .$ f = \a -> let !b = dbgId (f a) in g b

infixr 9 .$

dbgSTree :: (Show (v a), GM.MVector (G.Mutable v) a, G.Vector v a, PrimMonad m) => SegmentTree (G.Mutable v) (PrimState m) a -> m ()
dbgSTree (SegmentTree _ mVec) = do
  !vec <- G.unsafeFreeze mVec
  -- REMARK: I'm using 0-based index and it has 2^n - 1 vertices
  let !_ = dbg (G.drop (G.length vec `div` 2 - 1) vec)
  return ()

dbgSTreeAll :: (Show (v a), GM.MVector (G.Mutable v) a, G.Vector v a, PrimMonad m) => SegmentTree (G.Mutable v) (PrimState m) a -> m ()
dbgSTreeAll (SegmentTree _ mVec) = do
  !vec <- G.unsafeFreeze mVec
  flip fix (0 :: Int, 1 :: Int) $ \loop (!n, !len) -> do
    -- REMARK: I'm using 0-based index and it has 2^n - 1 vertices
    unless (G.length vec <= len) $ do
      let !vec' = G.take len . G.drop (len - 1) $ vec
      let !_ = dbgS $ "> " ++ show vec'
      loop (n + 1, 2 * len)

