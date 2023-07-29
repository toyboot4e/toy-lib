{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

-- | Debug utilities
module ToyLib.Debug where

import qualified Data.Vector.Generic as VG
import Data.Vector.IxVector

-- | For use with `dbgS`
class ShowGrid a where
  showGrid :: a -> String

instance (VG.Vector v a, Show a) => ShowGrid (IxVector (Int, Int) (v a)) where
  showGrid !grid = unlines $ map f [y0 .. y1]
    where
      ((!y0, !x0), (!y1, !x1)) = boundsIV grid
      f !y = unwords $ map (show . (grid @!) . (y,)) [x0 .. x1]

-- TODO: Array
