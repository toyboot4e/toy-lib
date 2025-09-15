{-# LANGUAGE CPP #-}
#include "./__import"
-- {{{ toy-lib import

import Data.Vector.IxVector
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB

-- }}} toy-lib import

{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug :: Bool
debug = False

-- | \(O(H + W)\) 2D index compression. NOT TESTED!!
compress2D :: U.Vector Int -> U.Vector Int -> (U.Vector Int, U.Vector Int)
compress2D xs ys = (xDict, yDict)
  where
    !xDict = U.uniq . U.modify VAI.sort $ xs U.++ U.map (+ 1) xs
    !yDict = U.uniq . U.modify VAI.sort $ ys U.++ U.map (+ 1) ys

-- | \(O(HW)\) Creates a 2D grid from inclusive rectangle add queries.
--
-- WARNING: Can you really allocate/run \(O(HW)\) algorithm?
imos2D :: (Num a) => U.Vector Int -> U.Vector Int -> U.Vector ((Int, Int), (Int, Int), a) -> IxUVector (Int, Int) a
imos2D xDict yDict input = runST $ do
  gr <- IxVector (zero2 h w) <$> UM.replicate (h * w) (0 :: Int)

  U.forM_ input $ \((!y1, !x1), (!y2, !x2), !d) -> do
    let !y1' = bindex xDict y1
    let !x1' = bindex xDict x1
    let !y2' = bindex xDict y2
    let !x2' = bindex xDict x2
    modifyIV gr (+ d) (y1', x1')
    modifyIV gr (-d) (y2' + 1, x1')
    modifyIV gr (-d) (y1', x2' + 1)
    modifyIV gr (+ d) (y2' + 1, x2' + 1)

  forM_ [0 .. h - 1] $ \y -> do
    forM_ [1 .. w - 1] $ \x -> do
      xl <- readIV gr (y, x - 1)
      modifyIV gr (+ xl) (y, x)

  forM_ [1 .. w - 1] $ \x -> do
    forM_ [0 .. h - 1] $ \y -> do
      yl <- readIV gr (y - 1, x)
      modifyIV gr (+ yl) (y, x)

  unsafeFreezeIV gr
  where
    w = G.length xDict
    h = G.length yDict

solve :: StateT BS.ByteString IO ()
solve = do
  n <- int'
  ldrus <- U.replicateM n ints4'

  let !xs = U.concatMap (\(!xl, !_, !xr, !_) -> U.fromListN 2 [xl, xr - 1]) ldrus
  let !ys = U.concatMap (\(!_, !yl, !_, !yr) -> U.fromListN 2 [yl, yr - 1]) ldrus
  let (!xDict, !yDict) = compress2D xs ys

  let !input = U.map (\(!xl, !yl, !xr, !yr) -> ((xl, yl), (xr - 1, yr - 1), 1 :: Int)) ldrus
  let !gr = imos2D xDict yDict input

  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/area_of_union_of_rectangles
-- #
main :: IO ()
main = runIO solve
