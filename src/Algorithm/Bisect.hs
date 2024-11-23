{-# LANGUAGE LambdaCase #-}

-- | \(O(\log N)\) bisection method for sorted items in an inclusive range (from left to right only).
--
-- `bisect` returns an @(yes, no)@ index pair at the boundary. `bisectL` and `bisectR` returns
-- one of the pair. `bisectM` is a monadic variant of `bisect`.
--
-- = Example
--
-- With an yes predicate @(<= 5)@, list @[0..9]@ can be seen as:
--
-- > [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
-- >  <-------------->  <-------->
-- >         yes             no
--
-- In the preceding example, `bisect` returns the @(yes, no)@ = @(Just 5, Just 6)@ pair at the boundary:
--
-- >>> :{
-- let xs = [0 :: Int .. 9]
--  in bisect 0 9 (\i -> xs !! i <= 5)
-- :}
-- (Just 5,Just 6)
--
-- `bisectL` returns @Just 5@ and `bisectR` returns @Just 6@.
module Algorithm.Bisect where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Functor.Identity
import Data.Maybe
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM

-- TODO: Use higher order function for getting middle and detecting end
-- TODO: quickcheck for `Double`

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

-- | @getMid@ parameter of `bisectImpl` for `Int` indices.
getMidInt :: Int -> Int -> Maybe Int
getMidInt l r
  -- TODO: do we need the @abs@?
  | abs (r - l) == 1 = Nothing
  | otherwise = Just $ (l + r) `div` 2

-- | @getMid@ parameter of `bisectImpl` for `Double` indices.
getMidDouble :: Double -> Double -> Double -> Maybe Double
getMidDouble eps l r
  -- TODO: do we need the @abs@?
  | abs (r - l) < eps = Nothing
  | otherwise = Just $ (l + r) / 2

-- * Bisection method for @Int@ range

-- | \(O(f \log N)\) Monadic binary search for an `Int` range.
{-# INLINE bisectM #-}
bisectM :: forall m. (Monad m) => Int -> Int -> (Int -> m Bool) -> m (Maybe Int, Maybe Int)
bisectM !l !r !p
  | l <= r = bisectImpl getMidInt l r (l - 1) (r + 1) p
  -- l > r is allowed
  | otherwise = bisectImpl getMidInt l r (l + 1) (r - 1) p

-- | \(O(f \log N)\)
{-# INLINE bisectML #-}
bisectML :: forall m. (Monad m) => Int -> Int -> (Int -> m Bool) -> m (Maybe Int)
bisectML !l !r !p = fst <$> bisectM l r p

-- | \(O(f \log N)\)
{-# INLINE bisectMR #-}
bisectMR :: forall m. (Monad m) => Int -> Int -> (Int -> m Bool) -> m (Maybe Int)
bisectMR !l !r !p = snd <$> bisectM l r p

-- | \(O(f \log N)\) Pure binary search for an `Int` range.
{-# INLINE bisect #-}
bisect :: Int -> Int -> (Int -> Bool) -> (Maybe Int, Maybe Int)
bisect !l !r !p = runIdentity $ bisectM l r (pure . p)

-- | \(O(f \log N)\) Also known as lower bound.
{-# INLINE bisectL #-}
bisectL :: Int -> Int -> (Int -> Bool) -> Maybe Int
bisectL !l !r !p = fst $! bisect l r p

-- | \(O(f \log N)\) Also known as upper bound.
{-# INLINE bisectR #-}
bisectR :: Int -> Int -> (Int -> Bool) -> Maybe Int
bisectR !l !r !p = snd $! bisect l r p

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

-- * Binary search over a vector

-- | \(O(f \log N)\) `bisectM` over a vector.
{-# INLINE bsearchM #-}
bsearchM :: (PrimMonad m, GM.MVector v a) => v (PrimState m) a -> (a -> Bool) -> m (Maybe Int, Maybe Int)
bsearchM !vec !p = bisectM 0 (GM.length vec - 1) (fmap p . GM.read vec)

-- | \(O(f \log N)\) `bisectML` over a vector.
{-# INLINE bsearchML #-}
bsearchML :: (PrimMonad m, GM.MVector v a) => v (PrimState m) a -> (a -> Bool) -> m (Maybe Int)
bsearchML !vec !p = bisectML 0 (GM.length vec - 1) (fmap p . GM.read vec)

-- | \(O(f \log N)\) `bisectMR` over a vector.
{-# INLINE bsearchMR #-}
bsearchMR :: (PrimMonad m, GM.MVector v a) => v (PrimState m) a -> (a -> Bool) -> m (Maybe Int)
bsearchMR !vec !p = bisectMR 0 (GM.length vec - 1) (fmap p . GM.read vec)

-- | \(O(\log N)\) `bsearchMExact` over a vector, searching for a specific value.
-- FIXME: It's slower than `bsearchL`.
{-# INLINE bsearchMExact #-}
bsearchMExact :: (PrimMonad m, GM.MVector v a, Ord b) => v (PrimState m) a -> (a -> b) -> b -> m (Maybe Int)
bsearchMExact !vec f !xref =
  bisectML 0 (GM.length vec - 1) (fmap ((<= xref) . f) . GM.read vec) >>= \case
    Just !i -> do
      !x <- f <$> GM.read vec i
      if x == xref
        then pure $ Just i
        else pure Nothing
    _ -> pure Nothing

-- | \(O(f \log N)\) `bisect` over a vector.
{-# INLINE bsearch #-}
bsearch :: (G.Vector v a) => v a -> (a -> Bool) -> (Maybe Int, Maybe Int)
bsearch !vec !p = bisect 0 (G.length vec - 1) (p . (vec G.!))

-- | \(O(f \log N)\) `bisectL` over a vector.
{-# INLINE bsearchL #-}
bsearchL :: (G.Vector v a) => v a -> (a -> Bool) -> Maybe Int
bsearchL !vec !p = bisectL 0 (G.length vec - 1) (p . (vec G.!))

-- | \(O(f \log N)\) `bisectR` over a vector.
{-# INLINE bsearchR #-}
bsearchR :: (G.Vector v a) => v a -> (a -> Bool) -> Maybe Int
bsearchR !vec !p = bisectR 0 (G.length vec - 1) (p . (vec G.!))

-- | \(O(\log N)\) `bsearchExact` over a vector, searching for a specific value.
-- FIXME: It's slower than `bsearchL`.
{-# INLINE bsearchExact #-}
bsearchExact :: (G.Vector v a, Ord b) => v a -> (a -> b) -> b -> Maybe Int
bsearchExact !vec f !xref = case bisectL 0 (G.length vec - 1) ((<= xref) . f . (vec G.!)) of
  Just !x | f (vec G.! x) == xref -> Just x
  _ -> Nothing

-- | Retrieves square root of an `Int`.
isqrtSlow :: Int -> Int
isqrtSlow n = fromJust $ bisectR 0 n ((< n) . (^ (2 :: Int)))
