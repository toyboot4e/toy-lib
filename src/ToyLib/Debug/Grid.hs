-- | Debug utilities
module ToyLib.Debug.Grid where

import Control.Monad
import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector.Generic as G
import Data.Vector.IxVector
import Debug.Trace
import ToyLib.Debug
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

-- | Shows grid in a human-readable spacing.
dbgGrid :: (ShowGrid a) => a -> ()
dbgGrid gr = dbgS (showGrid gr)

-- | Monadic variant of `dbgGrid`.
dbgGridM :: (PrimMonad m, G.Vector v a, Show a, ShowGrid (IxVector (Int, Int) (v a))) => IxVector (Int, Int) (G.Mutable v (PrimState m) a) -> m ()
-- TODO: simplifiable instance?
dbgGridM gr = when debug $ do
  !gr' <- unsafeFreezeIV gr
  let !_ = trace (showGridN 4 gr') ()
  pure ()

-- | Shows grid in a human-readable spacing.
dbgGridId :: (ShowGrid a) => a -> a
dbgGridId gr = let !_ = dbgS (showGrid gr) in gr

-- | Shows grid with the specified spacing.
dbgGridN :: (ShowGrid a) => Int -> a -> ()
dbgGridN len gr = dbgS (showGridN len gr)

-- | Shows grid with the specified spacing.
dbgGridNId :: (ShowGrid a) => Int -> a -> a
dbgGridNId len gr = let !_ = dbgS (showGridN len gr) in gr
