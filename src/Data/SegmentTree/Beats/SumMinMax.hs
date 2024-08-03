-- | `SumMinMax` and `AddChminChmax` on the segment tree beats.
--
-- = Typical problems
-- - [Range Chmin Chmax Add Range Sum](https://judge.yosupo.jp/problem/range_chmin_chmax_add_range_sum)
module Data.SegmentTree.Beats.SumMinMax where

import Control.Monad.Trans.State.Strict (execState, modify')
import Data.SegmentTree.Util (FailableSemigroupActionTarget(..), SemigroupActionWithLength (..))
import ToyLib.Debug

-- | Sum + Min + Max information on segment tree beats.
--
-- TODO: Specialize and `UNPACK`.
data SumMinMax a = SumMinMax
  { sumB :: !a,
    minB :: !a,
    maxB :: !a,
    -- | The number of elements that equals to `minB` in the interval.
    nMinB :: !Int,
    -- | The number of elements that equals to `maxB` in the interval.
    nMaxB :: !Int,
    -- | Secondary minimum value.
    min2B :: !a,
    -- | Secondary maximum value.
    max2B :: !a,
    failsB :: !Bool
  }
  deriving (Show, Eq)

instance FailableSemigroupActionTarget (SumMinMax a) where
  {-# INLINE isFailureFSAT #-}
  isFailureFSAT = failsB

{-# INLINE singletonSMM #-}
singletonSMM :: a -> SumMinMax a
singletonSMM x = SumMinMax x x x 1 1 x x False

instance (Num a, Ord a) => Semigroup (SumMinMax a) where
  -- not inline?
  {-# INLINE (<>) #-}
  x <> y
    -- when x is mempty
    | minB x > maxB x = y
    -- when y is mempty
    | minB y > maxB y = x
    | otherwise =
        let sum' = sumB x + sumB y
            min' = minB x + minB y
            max' = maxB x + maxB y
            nMax' = case compare (minB x) (minB y) of
              LT -> nMaxB x
              GT -> nMaxB y
              EQ -> nMaxB x + nMaxB y
            nMin' = case compare (maxB x) (maxB y) of
              LT -> nMinB x
              GT -> nMinB y
              EQ -> nMinB x + nMinB y
            min2' = (`execState` max') $ do
              -- chmin2 folding manually unrolled:
              modify' $ chmin2 min' (minB x)
              modify' $ chmin2 min' (min2B x)
              modify' $ chmin2 min' (minB y)
              modify' $ chmin2 min' (min2B y)
            max2' = (`execState` min') $ do
              -- chmax2 folding manually unrolled:
              modify' $ chmax2 (maxB x) max'
              modify' $ chmax2 (max2B x) max'
              modify' $ chmax2 (maxB y) max'
              modify' $ chmax2 (max2B y) max'
         in SumMinMax sum' min' max' nMin' nMax' min2' max2' False
    where
      chmin2 l_ r_ x_
        | l_ < r_ && r_ < x_ = r_
        | otherwise = x_
      chmax2 = chmin2

instance (Num a, Ord a, Bounded a) => Monoid (SumMinMax a) where
  {-# INLINE mempty #-}
  mempty =
    SumMinMax
      { sumB = 0,
        minB = maxBound,
        maxB = minBound,
        min2B = maxBound,
        max2B = minBound,
        nMinB = 0,
        nMaxB = 0,
        failsB = False
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

instance (Num a, Ord a) => Semigroup (AddChminChmax a) where
  -- TODO: not inline?
  {-# INLINE (<>) #-}
  (AddChminChmax a1 min1 max1) <> (AddChminChmax a2 min2 max2) =
    AddChminChmax (a1 + a2) (min min1 (min2 + a1)) (max max1 (max2 + a1))

instance (Num a, Ord a, Bounded a) => Monoid (AddChminChmax a) where
  {-# INLINE mempty #-}
  mempty = AddChminChmax 0 maxBound minBound

instance (Num a, Ord a, Bounded a) => SemigroupActionWithLength (AddChminChmax a) (SumMinMax a) where
  -- TODO: not inline?
  {-# INLINE sactWithLength #-}
  sactWithLength (AddChminChmax aAdd aMin aMax) x len
    -- when @x@ is identity element
    | minB x > maxB x = x
    -- The operator is add only
    | aMin == maxBound && aMax == minBound =
        x {sumB = sum', minB = min', min2B = min2', maxB = max', max2B = max2'}
    -- There's only one different value
    | min'' == max'' = singleton min'' len
    -- There are only two different values: min'' < max''
    | max2' <= min'' = twoValues min'' (len - nMaxB x) max'' (nMaxB x)
    -- There are only two different values: max'' < min''
    | min2' >= max'' = twoValues max'' (nMinB x) min'' (len - nMinB x)
    -- There are more than or equal to three different values
    | min'' < min2' && max2' < max'' =
        x
          { sumB = sum' + (min'' - min') * fromIntegral (nMinB x) + (max'' - max') * fromIntegral (nMaxB x),
            minB = min'',
            maxB = max'',
            min2B = min2',
            max2B = max2'
          }
    -- TODO: contradiction, when does it happen?
    -- TODO: Is it ok to use @mempty@?
    | otherwise = mempty {failsB = True}
    where
      !_ = dbgAssert (not (failsB x)) "AddChminChmax applied while it's failed"
      -- after add
      sum' = sumB x + aAdd * fromIntegral len
      min' = minB x + aAdd
      max' = maxB x + aAdd
      min2' = min2B x + aAdd
      max2' = max2B x + aAdd
      -- apply chmin and chmax
      min'' = min aMax . max aMin $ min'
      max'' = min aMax . max aMin $ max'
      -- constructors:
      singleton v len_ =
        SumMinMax
          { sumB = v * fromIntegral len_,
            minB = v,
            maxB = v,
            nMinB = len_,
            nMaxB = len_,
            min2B = v,
            max2B = v,
            failsB = False
          }
      twoValues v1 n1 v2 n2 =
        -- v1 < v2
        SumMinMax
          { sumB = v1 * fromIntegral n1 + v2 * fromIntegral n2,
            minB = v1,
            maxB = v2,
            nMinB = n1,
            nMaxB = n2,
            min2B = v2,
            max2B = v1,
            failsB = False
          }

-- type SegmentTreeBeats k a op s = LazySegmentTree (SumMinMax k a) op s

-- type SegmentTreeBeats k a op s = LazySegmentTree (SumMinMax k a) op s

-- _updateMinLSTree :: (PrimMonad m) => SegmentTreeBeats k a op (PrimState m) -> Int -> k -> m ()
-- _updateMinLSTree stree i x = do
--   return ()

-- -- | Updates chmax/chmin
-- chLSTree ::
