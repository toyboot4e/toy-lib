-- | `ShowBSB` implementation for grids.
module ToyLib.ShowBSB.Grid where

import Control.Monad.IO.Class
import qualified Data.ByteString.Builder as BSB
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import Data.Vector.IxVector
import qualified Data.Vector.Unboxed as U
import ToyLib.ShowBSB

printGrid :: (MonadIO m) => IxUVector (Int, Int) Char -> m ()
printGrid = putBSB . showGridBSB

showGridBSB :: IxUVector (Int, Int) Char -> BSB.Builder
showGridBSB mat = G.foldMap ((<> endlBSB) . concatBSB) rows
  where
    ((!y1, !x1), (!y2, !x2)) = boundsIV mat
    !h = y2 + 1 - y1
    !w = x2 + 1 - x1
    rows = V.unfoldrExactN h (U.splitAt w) (vecIV mat)

printMat :: (ShowBSB a, U.Unbox a, MonadIO m) => IxUVector (Int, Int) a -> m ()
printMat = putBSB . showMatBSB

showMatBSB :: (ShowBSB a, U.Unbox a) => IxUVector (Int, Int) a -> BSB.Builder
showMatBSB mat = G.foldMap ((<> endlBSB) . unwordsBSB) rows
  where
    ((!y1, !x1), (!y2, !x2)) = boundsIV mat
    !h = y2 + 1 - y1
    !w = x2 + 1 - x1
    rows = V.unfoldrExactN h (U.splitAt w) (vecIV mat)
