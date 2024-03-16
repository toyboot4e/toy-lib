{-# LANGUAGE TypeFamilies #-}

-- | Vector-based matrix implementation.
module Math.Matrix where

import Data.BinaryLifting
import Data.Core.SemigroupAction
import qualified Data.Vector as V
import Data.Vector.Extra (chunksOfG)
import qualified Data.Vector.Generic as G
import Data.Vector.IxVector
import qualified Data.Vector.Unboxed as U
import GHC.Stack (HasCallStack)
import ToyLib.Debug
import ToyLib.Prelude (zero2)

-- | HxW matrix.
type Mat a = IxUVector (Int, Int) a

-- | Column vector.
type Col a = U.Vector a

-- | Multiplies HxW matrix to a Hx1 column vector.
mulMatToCol :: (HasCallStack, Num e, U.Unbox e) => Mat e -> Col e -> Col e
mulMatToCol !mat !col = U.convert $ G.map (G.sum . flip (G.zipWith (*)) col) rows
  where
    !n = G.length col
    !_ = dbgAssert $ (== n) . succ . fst . snd $ boundsIV mat
    rows = chunksOfG n (vecIV mat)

-- | Multiplies HxW matrix to a Hx1 column vector, taking the modulus.
mulMatToColMod :: (HasCallStack, Num e, U.Unbox e, Integral e) => e -> Mat e -> Col e -> Col e
mulMatToColMod !modulus !mat !col = U.convert $ G.map (G.foldl' addMod_ 0 . flip (G.zipWith mulMod_) col) rows
  where
    !n = G.length col
    !_ = dbgAssert $ (== n) . succ . fst . snd $ boundsIV mat
    rows = chunksOfG n (vecIV mat)
    addMod_ x y = (x + y) `mod` modulus
    mulMod_ x y = (x * y) `mod` modulus

-- | Multiplies H1xK matrix to a KxW2 matrix.
mulMat :: (HasCallStack, Num e, U.Unbox e) => Mat e -> Mat e -> Mat e
mulMat !a !b = generateIV (zero2 w' h) $ \(!row, !col) ->
  U.sum $ U.zipWith (*) (rows1 V.! row) (cols2 V.! col)
  where
    ((!x1, !y1), (!x2, !y2)) = boundsIV a
    w = x2 + 1 - x1
    h = y2 + 1 - y1
    ((!x1', !y1'), (!x2', !y2')) = boundsIV a
    w' = x2' + 1 - x1'
    h' = y2' + 1 - y1'
    !_ = dbgAssert (w == h') $ "matrix size mismatch: " ++ show (boundsIV a) ++ " - " ++ show (boundsIV b)
    rows1 = chunksOfG w (vecIV a)
    cols2 = V.generate w' $ \col -> U.generate h' $ \row -> vecIV b U.! (w' * row + col)

-- | Multiplies H1xK matrix to a KxW2 matrix, taking the modulus.
mulMatMod :: (HasCallStack, Num e, U.Unbox e, Integral e) => e -> Mat e -> Mat e -> Mat e
mulMatMod !m !a !b = generateIV (zero2 w' h) $ \(!row, !col) ->
  U.foldl' addMod_ 0 $ U.zipWith mulMod_ (rows1 V.! row) (cols2 V.! col)
  where
    ((!x1, !y1), (!x2, !y2)) = boundsIV a
    w = x2 + 1 - x1
    h = y2 + 1 - y1
    ((!x1', !y1'), (!x2', !y2')) = boundsIV a
    w' = x2' + 1 - x1'
    h' = y2' + 1 - y1'
    !_ = dbgAssert (w == h') $ "matrix size mismatch: " ++ show (boundsIV a) ++ " - " ++ show (boundsIV b)
    rows1 = chunksOfG w (vecIV a)
    cols2 = V.generate w' $ \col -> U.generate h' $ \row -> vecIV b U.! (w' * row + col)
    addMod_ x y = (x + y) `mod` m
    mulMod_ x y = (x * y) `mod` m

-- | Retruns NxN unit matrix.
{-# INLINE unitMat #-}
unitMat :: (U.Unbox e, Num e) => Int -> Mat e
unitMat !n = constructIV (zero2 n n) $ \_ (!row, !col) -> if col == row then 1 else 0

instance (Num a, U.Unbox a) => Semigroup (Mat a) where
  {-# INLINE (<>) #-}
  (<>) = mulMat

instance (Num a, U.Unbox a) => SemigroupAction (Mat a) (Col a) where
  {-# INLINE sact #-}
  sact = mulMatToCol

instance (Num a, U.Unbox a) => BinaryLifting (Mat a) where
  type VecBL (Mat a) = V.Vector (Mat a)
  {-# INLINE cacheBL #-}
  cacheBL = cacheBLV
