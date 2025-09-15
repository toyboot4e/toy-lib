{-# LANGUAGE LambdaCase #-}

-- FIXME: Always use half-open interval

-- | \(O(\log N)\) bisection method for sorted items in an inclusive range (from left to right only).
module Algorithm.Bisect where

import Data.Functor.Identity

-- * Common bisection method implementation

-- | \(O(f \log N)\) Bisection method implementation. It's generalized over both the index type
-- and the monad.
--
-- = Parameters
-- - @getMid@: Returns the mid index between two. Returns @Nothing@ if the two indices are close enough.
-- - @lowOut@: Nearest low index that is NOT included in the range.
-- - @highOut@: Nearest high index that is NOT included in the range.
bisectImpl :: forall i m. (Eq i, Monad m) => (i -> i -> Maybe i) -> i -> i -> i -> i -> (i -> m Bool) -> m (Maybe i, Maybe i)
bisectImpl getMid l0 r0 lowOut highOut p = done <$> inner lowOut highOut
  where
    done :: (i, i) -> (Maybe i, Maybe i)
    done (!l, !r)
      | l == lowOut = (Nothing, Just l0)
      | r == highOut = (Just r0, Nothing)
      | otherwise = (Just l, Just r)
    inner :: i -> i -> m (i, i)
    inner !y !n
      | Just m <- getMid y n =
          p m >>= \case
            True -> inner m n
            False -> inner y m
      | otherwise = pure (y, n)

-- | @getMid@ parameter of `bisectImpl` for `Double` indices.
getMidDouble :: Double -> Double -> Double -> Maybe Double
getMidDouble eps l r
  -- TODO: do we need the @abs@?
  | abs (r - l) < eps = Nothing
  | otherwise = Just $ (l + r) / 2

-- * Bisection method for @Double@ range

-- REMARK: Is your `eps` small enough?
-- let !eps = 10.0 ** (-12.0) :: Double

-- | \(O(f \log N)\) Monadic binary search for an `Double` range.
{-# INLINE bisectMF64 #-}
bisectMF64 :: forall m. (Monad m) => Double -> Double -> Double -> (Double -> m Bool) -> m (Maybe Double, Maybe Double)
bisectMF64 !eps !l !r !p
  | l <= r = bisectImpl (getMidDouble eps) l r (l - eps) (r + eps) p
  -- l > r is allowed
  | otherwise = bisectImpl (getMidDouble eps) l r (l + eps) (r - eps) p

-- | \(O(f \log N)\)
{-# INLINE bisectMLF64 #-}
bisectMLF64 :: forall m. (Monad m) => Double -> Double -> Double -> (Double -> m Bool) -> m (Maybe Double)
bisectMLF64 !eps !l !r !p = fst <$> bisectMF64 eps l r p

-- | \(O(f \log N)\)
{-# INLINE bisectMRF64 #-}
bisectMRF64 :: forall m. (Monad m) => Double -> Double -> Double -> (Double -> m Bool) -> m (Maybe Double)
bisectMRF64 !eps !l !r !p = snd <$> bisectMF64 eps l r p

-- | \(O(f \log N)\) Pure binary search for an `Double` range.
{-# INLINE bisectF64 #-}
bisectF64 :: Double -> Double -> Double -> (Double -> Bool) -> (Maybe Double, Maybe Double)
bisectF64 !eps !l !r !p = runIdentity $ bisectMF64 eps l r (pure . p)

-- | \(O(f \log N)\) Also known as lower bound.
{-# INLINE bisectLF64 #-}
bisectLF64 :: Double -> Double -> Double -> (Double -> Bool) -> Maybe Double
bisectLF64 !eps !l !r !p = fst $! bisectF64 eps l r p

-- | \(O(f \log N)\) Also known as upper bound.
{-# INLINE bisectRF64 #-}
bisectRF64 :: Double -> Double -> Double -> (Double -> Bool) -> Maybe Double
bisectRF64 !eps !l !r !p = snd $! bisectF64 eps l r p
