{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

-- | TODO: Refactor in my way.

module Math.Matrix where

import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import ToyLib.Macro

-- {{{ Math

mulMat :: (Num e, IArray UArray e) => UArray (Int, Int) e -> UArray (Int, Int) e -> UArray (Int, Int) e
mulMat a b =
  listArray @UArray
    ((i0, k0), (ix, kx))
    [ sum [a ! (i, j) * b ! (j', k) | (j, j') <- zip (range (j0, jx)) (range (j'0, j'x))]
      | i <- range (i0, ix),
        k <- range (k0, kx)
    ]
  where
    ((i0, j0), (ix, jx)) = bounds a
    ((j'0, k0), (j'x, kx)) = bounds b
    !_ = dbgAssert (jx - j0 == j'x - j'0)

mulMatMod :: Int -> UArray (Int, Int) Int -> UArray (Int, Int) Int -> UArray (Int, Int) Int
mulMatMod m a b =
  listArray @UArray
    ((i0, k0), (ix, kx))
    [ sum [a ! (i, j) * b ! (j', k) `mod` m | (j, j') <- zip (range (j0, jx)) (range (j'0, j'x))] `mod` m
      | i <- range (i0, ix),
        k <- range (k0, kx)
    ]
  where
    ((i0, j0), (ix, jx)) = bounds a
    ((j'0, k0), (j'x, kx)) = bounds b
    !_ = dbgAssert (jx - j0 == j'x - j'0)

-- }}}
