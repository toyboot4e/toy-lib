-- | The Imos's algorithm template for 2D grids.
module Algorithm.Imos where

import Control.Monad
import Data.Array.IArray
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed (UArray)
import ToyLib.Prelude

-- {{{ imos 2D

-- | WARNING: Can you really allocate/run \(O(HW)\) algorithm?
imos2D :: ((Int, Int), (Int, Int)) -> UArray (Int, Int) Int -> UArray (Int, Int) Int
imos2D !bounds_ !seeds = runSTUArray $ do
  !arr <- newArray bounds_ (0 :: Int)

  let (!minY, !minX) = fst bounds_

  -- row scan
  forM_ (range bounds_) $ \(!y, !x) -> do
    !v <- if x == minX then return 0 else readArray arr (y, x - 1)
    let !diff = seeds ! (y, x)
    writeArray arr (y, x) $! v + diff

  -- column scan
  forM_ (range bounds_) $ \(!x, !y) -> do
    !v <- if y == minY then return 0 else readArray arr (y - 1, x)
    !diff <- readArray arr (y, x)
    writeArray arr (y, x) $! v + diff

  return arr

-- | WARNING: Can you really allocate/run \(O(HW)\) algorithm?
imos2DRev :: ((Int, Int), (Int, Int)) -> UArray (Int, Int) Int -> UArray (Int, Int) Int
imos2DRev !bounds_ !seeds = runSTUArray $ do
  !arr <- newArray bounds_ (0 :: Int)

  let (!minY, !minX) = fst bounds_
  let (!maxY, !maxX) = snd bounds_

  -- row scan
  -- forM_ (reverse $ range bounds_) $ \(!y, !x) -> do
  forM_ [maxX, maxX - 1 .. minX] $ \x -> do
    forM_ [maxY, maxY - 1 .. minY] $ \y -> do
      !v <- if x == maxX then return 0 else readArray arr (y, x + 1)
      let !diff = seeds ! (y, x)
      writeArray arr (y, x) $! v + diff

  -- column scan
  forM_ [maxX, maxX - 1 .. minX] $ \x -> do
    forM_ [maxY, maxY - 1 .. minY] $ \y -> do
      !v <- if y == maxY then return 0 else readArray arr (y + 1, x)
      !diff <- readArray arr (y, x)
      writeArray arr (y, x) $! v + diff

  return arr

-- }}}
