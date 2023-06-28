{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

-- | TODO: Refactor in my way.

module Math.Matrix where

import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import ToyLib.Macro
import Data.BinaryLifting

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

unitMat :: Int -> UArray (Int, Int) Int
unitMat n = accumArray @UArray (+) (0 :: Int) ((0, 0), (pred n, pred n)) $ map ((,1) . dupe) [0 .. pred n]

-- | `mulMatMod` wrapper for binary lifting.
--
-- > let !m1 = accumArray @UArray (-) (1 :: Int) ((0, 0), (pred nVerts, pred nVerts)) $ map (,1) removals'
-- > let !mn = newBinLiftV $ MulMatMod @MyModulus m1
-- > let MulMatMod !mk = stimesBL mn (MulMatMod $ unitMat nVerts) (lenPath)
newtype MulMatMod a = MulMatMod (UArray (Int, Int) Int)
  deriving (Eq, Show)

instance TypeInt p => Semigroup (MulMatMod p) where
  (MulMatMod !m1) <> (MulMatMod !m2) = MulMatMod $ mulMatMod (typeInt (Proxy @p)) m1 m2

-- }}}
