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
  qs <- U.replicateM q $ do
    int' >>= \case
      0 -> (0 :: Int,,,,-1 :: Int) <$> int' <*> int' <*> int'
      1 -> (1 :: Int,,,,) <$> int' <*> int' <*> int' <*> int'
      _ -> error "unreachable"

  let xys =
        U.map (\(!x, !y, !_) -> (x, y)) xyws
          U.++ U.mapMaybe
            ( \case
                (0, !x, !y, !_, !_) -> Just (x, y)
                _ -> Nothing
            )
            qs

  wm <- buildWMST xys
  U.forM_ xyws $ \(!x, !y, !w) -> do
    modifyWMST wm (<> Sum w) (x, y)

  res <-
    U.mapMaybeM
      ( \case
          (0, !x, !y, !w, !_) -> do
            modifyWMST wm (<> Sum w) (x, y)
            return Nothing
          (1, !xl, !yl, pred -> !xr, pred -> !yr) -> do
            Just . getSum . fromMaybe mempty <$> foldMayWMST wm xl xr yl yr
          _ -> error "unreachable"
      )
      qs

  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/point_add_rectangle_sum
-- #wavelet-matrix-2d
main :: IO ()
main = runIO solve
