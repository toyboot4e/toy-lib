{-# LANGUAGE MagicHash #-}

-- | TODO: Refactor in my way.
module Math.Matrix where

import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.Core.SemigroupAction
import Data.List (foldl1')
import Data.List.Extra (chunksOf)
import Data.Tuple.Extra (dupe)
import GHC.Exts
import GHC.TypeLits
import ToyLib.Debug
import ToyLib.Prelude ((.:))

-- | Multiplies HxW matrix to a Hx1 column vector.
mulMatToCol :: (Num e, IArray UArray e) => UArray (Int, Int) e -> [e] -> [e]
mulMatToCol !mat !col =
  let !rows = chunksOf n (elems mat)
   in map (sum . zipWith (*) col) rows
  where
    !n = length col
    !_ = dbgAssert $ (== n) . succ . fst . snd $ bounds mat

mulMatToColMod :: UArray (Int, Int) Int -> Int -> [Int] -> [Int]
mulMatToColMod !mat !modulus !col =
  let !rows = chunksOf n (elems mat)
   in map (foldl1' ((`mod` modulus) .: (+)) . zipWith (*) col) rows
  where
    !n = length col
    !_ = dbgAssert $ (== n) . succ . fst . snd $ bounds mat

-- | Multiplies H1xK matrix to a KxW2 matrix.
mulMat :: (Num e, IArray UArray e) => UArray (Int, Int) e -> UArray (Int, Int) e -> UArray (Int, Int) e
mulMat !a !b =
  listArray @UArray
    ((i0, k0), (ix, kx))
    [ sum [a ! (i, j) * b ! (j', k) | (j, j') <- zip (range (j0, jx)) (range (j'0, j'x))]
      | i <- range (i0, ix),
        k <- range (k0, kx)
    ]
  where
    ((!i0, !j0), (!ix, !jx)) = bounds a
    ((!j'0, !k0), (!j'x, !kx)) = bounds b
    !_ = dbgAssert (jx - j0 == j'x - j'0)

-- | Multiplies H1xK matrix to a KxW2 matrix, getting mod of @m@.
mulMatMod :: Int -> UArray (Int, Int) Int -> UArray (Int, Int) Int -> UArray (Int, Int) Int
mulMatMod m a b =
  listArray @UArray
    ((i0, k0), (ix, kx))
    [ sum [a ! (i, j) * b ! (j', k) `mod` m | (j, j') <- zip (range (j0, jx)) (range (j'0, j'x))] `mod` m
      | i <- range (i0, ix),
        k <- range (k0, kx)
    ]
  where
    ((!i0, !j0), (!ix, !jx)) = bounds a
    ((!j'0, !k0), (!j'x, !kx)) = bounds b
    !_ = dbgAssert (jx - j0 == j'x - j'0)

-- | Retruns NxN unit matrix.
unitMat :: Int -> UArray (Int, Int) Int
unitMat !n = accumArray @UArray (+) (0 :: Int) ((0, 0), (pred n, pred n)) $ map ((,1) . dupe) [0 .. pred n]

-- | `mulMatMod` wrapper for binary lifting.
--
-- > let !m1 = accumArray @UArray (-) (1 :: Int) ((0, 0), (pred nVerts, pred nVerts)) $ map (,1) removals'
-- > let !mn = newBinLiftV $ MulMatMod @MyModulo m1
-- > let MulMatMod !mk = stimesBL mn (MulMatMod $ unitMat nVerts) (lenPath)
newtype MulMatMod p = MulMatMod (UArray (Int, Int) Int)
  deriving (Eq, Show)

-- Implementation over `Int` only!

instance forall p. (KnownNat p) => Semigroup (MulMatMod p) where
  (MulMatMod !m1) <> (MulMatMod !m2) = MulMatMod $ mulMatMod (fromInteger (natVal' (proxy# @p))) m1 m2

-- | Semigroup action over a list as a column vector
instance (KnownNat p) => SemigroupAction (MulMatMod p) [Int] where
  sact (MulMatMod !mat) !col = mulMatToColMod mat (fromInteger (natVal' (proxy# @p))) col

