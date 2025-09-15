{-# LANGUAGE CPP #-}
#include "./__import"
-- {{{ toy-lib import

import Data.WaveletMatrix.SegTree
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB

-- }}} toy-lib import

{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug :: Bool
debug = False

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !q) <- ints2'
  xyws <- U.replicateM n ints3'
  qs <- U.replicateM q ints4'

  wm <- buildWMST $ U.map (\(!x, !y, !_) -> (x, y)) xyws
  U.forM_ xyws $ \(!x, !y, !w) -> do
    modifyWMST wm (<> Sum w) (x, y)

  res <-
    U.mapM
      ( \(!xl, yl, pred -> !xr, pred -> !yr) -> do
          getSum . fromMaybe mempty <$> foldMayWMST wm xl xr yl yr
      )
      qs
  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/rectangle_sum
-- #wavelet-matrix-2d
main :: IO ()
main = runIO solve
