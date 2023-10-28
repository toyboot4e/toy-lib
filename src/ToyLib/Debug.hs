
-- | Debug utilities
module ToyLib.Debug where

import qualified Data.Vector.Generic as VG
import Data.Vector.IxVector
import ToyLib.Macro

-- | For use with `dbgS`
class ShowGrid a where
  showGrid :: a -> String

instance (VG.Vector v a, Show a) => ShowGrid (IxVector (Int, Int) (v a)) where
  showGrid !grid = unlines $ map f [y0 .. y1]
    where
      ((!y0, !x0), (!y1, !x1)) = boundsIV grid
      f !y = unwords $ map (show . (grid @!) . (y,)) [x0 .. x1]

-- | `$` with `dbgId`
($$) :: (Show a) => (a -> b) -> a -> b
($$) lhs rhs = lhs (dbgId rhs)
infixr 0 $$

-- | `.` with `dbgId`
(.$) :: Show b => (b -> c) -> (a -> b) -> a -> c
g .$ f = \a -> let !b = dbgId (f a) in g b
infixr 9 .$

-- TODO: Array
