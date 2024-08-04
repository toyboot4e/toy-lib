{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | `SumMinMax` and `AddChminChmax` on the segment tree beats.
--
-- = Typical problems
-- - [Range Chmin Chmax Add Range Sum](https://judge.yosupo.jp/problem/range_chmin_chmax_add_range_sum)
module Data.SegmentTree.Beats.SumMinMax where

import Control.Monad.Trans.State.Strict (execState, modify')
import Data.SegmentTree.Util (FailableSemigroupActionTarget (..), SemigroupActionWithLength (..))
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import ToyLib.Debug

-- | Sum + Min + Max information on segment tree beats.
--
-- TODO: Specialize and `UNPACK`.
data SumMinMax a = SumMinMax
  { sumSMM :: !a,
    minSMM :: !a,
    maxSMM :: !a,
    -- | The number of elements that equals to `minSMM` in the interval.
    nMinSMM :: !Int,
    -- | The number of elements that equals to `maxSMM` in the interval.
    nMaxSMM :: !Int,
    -- | Secondary minimum value.
    min2SMM :: !a,
    -- | Secondary maximum value.
    max2SMM :: !a,
    failsSMM :: !Bool
  }
  deriving (Show, Eq)

instance FailableSemigroupActionTarget (SumMinMax a) where
  {-# INLINE isFailureFSAT #-}
  isFailureFSAT = failsSMM

{-# INLINE singletonSMM #-}
singletonSMM :: a -> SumMinMax a
singletonSMM x = SumMinMax x x x 1 1 x x False

instance (Num a, Ord a) => Semigroup (SumMinMax a) where
  -- not inline?
  {-# INLINE (<>) #-}
  x <> y
    -- when x is mempty
    | minSMM x > maxSMM x = y
    -- when y is mempty
    | minSMM y > maxSMM y = x
    | otherwise =
        let sum' = sumSMM x + sumSMM y
            min' = minSMM x `min` minSMM y
            max' = maxSMM x `max` maxSMM y
            nMin' = case compare (minSMM x) (minSMM y) of
              LT -> nMinSMM x
              GT -> nMinSMM y
              EQ -> nMinSMM x + nMinSMM y
            nMax' = case compare (maxSMM x) (maxSMM y) of
              GT -> nMaxSMM x
              LT -> nMaxSMM y
              EQ -> nMaxSMM x + nMaxSMM y
            min2' = (`execState` max') $ do
              -- chmin2 folding manually unrolled:
              modify' $ chmin2 min' (minSMM x)
              modify' $ chmin2 min' (min2SMM x)
              modify' $ chmin2 min' (minSMM y)
              modify' $ chmin2 min' (min2SMM y)
            max2' = (`execState` min') $ do
              -- chmax2 folding manually unrolled:
              modify' $ chmax2 (maxSMM x) max'
              modify' $ chmax2 (max2SMM x) max'
              modify' $ chmax2 (maxSMM y) max'
              modify' $ chmax2 (max2SMM y) max'
         in SumMinMax sum' min' max' nMin' nMax' min2' max2' False
    where
      chmin2 l_ r_ x_
        | l_ < r_ && r_ < x_ = r_
        | otherwise = x_
      chmax2 l_ r_ x_
        | x_ < l_ && l_ < r_ = l_
        | otherwise = x_

instance (Num a, Ord a, Bounded a) => Monoid (SumMinMax a) where
  {-# INLINE mempty #-}
  mempty =
    SumMinMax
      { sumSMM = 0,
        minSMM = maxBound,
        maxSMM = minBound,
        nMinSMM = 0,
        nMaxSMM = 0,
        min2SMM = maxBound,
        max2SMM = minBound,
        failsSMM = False
      }

-- | Add first then apply chmin and chmax, works on segment tree beats.
--
-- TODO: Specialize and `UNPACK`.
data AddChminChmax a = AddChminChmax
  { addACC :: !a,
    chminACC :: !a,
    chmaxACC :: !a
  }
  deriving (Show, Eq)

{-# INLINE newAddACC #-}
newAddACC :: (Num a, Ord a, Bounded a) => a -> AddChminChmax a
newAddACC x = mempty {addACC = x}

{-# INLINE newChminACC #-}
newChminACC :: (Num a, Ord a, Bounded a) => a -> AddChminChmax a
newChminACC x = mempty {chminACC = x}

{-# INLINE newChmaxACC #-}
newChmaxACC :: (Num a, Ord a, Bounded a) => a -> AddChminChmax a
newChmaxACC x = mempty {chmaxACC = x}

instance (Num a, Ord a, Bounded a) => Semigroup (AddChminChmax a) where
  {-# INLINE (<>) #-}
  (AddChminChmax a1 min1 max1) <> (AddChminChmax a2 min2 max2) = AddChminChmax a' min' max'
    where
      -- TODO: handle identity elements implicitly (without making overflow)
      !a' = a1 + a2
      !min'
        | min2 == maxBound = min1
        | otherwise = min min1 (min2 + a1)
      !max'
        | max2 == minBound = max1
        | otherwise = max max1 (max2 + a1)

instance (Num a, Ord a, Bounded a) => Monoid (AddChminChmax a) where
  {-# INLINE mempty #-}
  mempty = AddChminChmax 0 maxBound minBound

instance (Num a, Ord a, Bounded a) => SemigroupActionWithLength (AddChminChmax a) (SumMinMax a) where
  -- TODO: not inline?
  {-# INLINE sactWithLength #-}
  sactWithLength (AddChminChmax !aAdd !aMin !aMax) !x len
    -- when @x@ is identity element
    | minSMM x > maxSMM x = x
    -- No chmin or chmax
    | aMin == maxBound && aMax == minBound =
        x {sumSMM = sum', minSMM = min', min2SMM = min2', maxSMM = max', max2SMM = max2'}
    -- There's only one different value
    | min'' == max'' = singleton min'' len
    -- There are only two different values: min'' < max''
    | max2' <= min'' = twoValues min'' (len - nMaxSMM x) max'' (nMaxSMM x)
    -- There are only two different values: max'' < min''
    | min2' >= max'' = twoValues min'' (nMinSMM x) max'' (len - nMinSMM x)
    -- There are four different values:
    | min'' < min2' && max2' < max'' =
        x
          { sumSMM = sum' + (min'' - min') * fromIntegral (nMinSMM x) + (max'' - max') * fromIntegral (nMaxSMM x),
            minSMM = min'',
            maxSMM = max'',
            min2SMM = min2',
            max2SMM = max2'
          }
    -- TODO: Contradiction on max2' <= chmin or chmax <= min2'?
    | otherwise = mempty {failsSMM = True}
    where
      !_ = dbgAssert (not (failsSMM x)) "AddChminChmax applied while it's failed"
      -- after add
      sum' = sumSMM x + aAdd * fromIntegral len
      min' = minSMM x + aAdd
      max' = maxSMM x + aAdd
      min2' = min2SMM x + aAdd
      max2' = max2SMM x + aAdd
      -- apply chmin and chmax
      min'' = max aMax . min aMin $ min'
      max'' = max aMax . min aMin $ max'
      -- constructors:
      singleton v len_ =
        SumMinMax
          { sumSMM = v * fromIntegral len_,
            minSMM = v,
            maxSMM = v,
            nMinSMM = len_,
            nMaxSMM = len_,
            min2SMM = v,
            max2SMM = v,
            failsSMM = False
          }
      twoValues v1 n1 v2 n2 =
        -- v1 < v2
        SumMinMax
          { sumSMM = v1 * fromIntegral n1 + v2 * fromIntegral n2,
            minSMM = v1,
            maxSMM = v2,
            nMinSMM = n1,
            nMaxSMM = n2,
            min2SMM = v2,
            max2SMM = v1,
            failsSMM = False
          }

-- * `Unbox` instances

-- vector provides unbox implementation up to 6-tuples
type SumMinMaxRepr a = (a, a, a, a, a, (Int, Int, Bool))

instance U.IsoUnbox (SumMinMax a) (SumMinMaxRepr a) where
  {-# INLINE toURepr #-}
  toURepr SumMinMax {..} = (sumSMM, minSMM, maxSMM, min2SMM, max2SMM, (nMinSMM, nMaxSMM, failsSMM))
  {-# INLINE fromURepr #-}
  fromURepr (!sumSMM, !minSMM, !maxSMM, !min2SMM, !max2SMM, (!nMinSMM, !nMaxSMM, !failsSMM)) = SumMinMax {..}

{- ORMOLU_DISABLE -}
newtype instance U.MVector s (SumMinMax a) = MV_SumMinMax (UM.MVector s (SumMinMaxRepr a))
newtype instance U.Vector (SumMinMax a) = V_SumMinMax (U.Vector (SumMinMaxRepr a))
deriving via (SumMinMax a `U.As` SumMinMaxRepr a) instance (U.Unbox a) => GM.MVector UM.MVector (SumMinMax a)
deriving via (SumMinMax a `U.As` SumMinMaxRepr a) instance (U.Unbox a) => G.Vector U.Vector (SumMinMax a)
instance (U.Unbox a) => U.Unbox (SumMinMax a)
{- ORMOLU_ENABLE -}

-- vector provides unbox implementation up to 6-tuples
type AddChminChmaxRepr a = (a, a, a)

instance U.IsoUnbox (AddChminChmax a) (AddChminChmaxRepr a) where
  {-# INLINE toURepr #-}
  toURepr AddChminChmax {..} = (addACC, chminACC, chmaxACC)
  {-# INLINE fromURepr #-}
  fromURepr (!addACC, !chminACC, !chmaxACC) = AddChminChmax {..}

{- ORMOLU_DISABLE -}
newtype instance U.MVector s (AddChminChmax a) = MV_AddChminChmax (UM.MVector s (AddChminChmaxRepr a))
newtype instance U.Vector (AddChminChmax a) = V_AddChminChmax (U.Vector (AddChminChmaxRepr a))
deriving via (AddChminChmax a `U.As` AddChminChmaxRepr a) instance (U.Unbox a) => GM.MVector UM.MVector (AddChminChmax a)
deriving via (AddChminChmax a `U.As` AddChminChmaxRepr a) instance (U.Unbox a) => G.Vector U.Vector (AddChminChmax a)
instance (U.Unbox a) => U.Unbox (AddChminChmax a)
{- ORMOLU_ENABLE -}
