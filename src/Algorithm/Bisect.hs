{-# LANGUAGE LambdaCase #-}

-- | \(O(\log N)\) bisection method for sorted items in an inclusive range (from left to right only).
--
-- `bisect` returns an @(ok, ng)@ index pair at the boundary. `bisectL` and `bisectR` returns
-- one of the pair. `bisectM` is a monadic variant of `bisect`.
--
-- = Example
--
-- With an OK predicate @(<= 5)@, list @[0..9]@ can be seen as:
--
-- > [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
-- >  <-------------->  <-------->
-- >         ok             ng
--
-- In the preceding example, `bisect` returns the @(ok, ng)@ = @(Just 5, Just 6)@ pair at the boundary:
--
-- >>> :{
-- let xs = [0 :: Int .. 9]
--  in bisect (0 :: Int, 9 :: Int) (\i -> xs !! i <= 5)
-- :}
-- (Just 5,Just 6)
--
-- `bisectL` returns @Just 5@ and `bisectR` returns @Just 6@.
module Algorithm.Bisect
  ( -- * Bisection methods

    -- ** `Int`
    bisect,
    bisectL,
    bisectR,
    bisectM,
    bisectML,
    bisectMR,

    -- ** `Double`
    bisectF64,
    bisectF64L,
    bisectF64R,

    -- * Binary search over vector
    bsearch,
    bsearchL,
    bsearchR,
    bsearchExact,
    bsearchM,
    bsearchML,
    bsearchMR,
    bsearchMExact,

    -- * Misc
    isqrtSlow,
  )
where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Functor.Identity
import Data.Ix
import Data.Maybe
import Data.Tuple.Extra hiding (first, second)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM

-- TODO: Use higher order function for getting middle and detecting end

-- | \(O(f \log N)\) Pure binary search.
{-# INLINE bisect #-}
bisect :: Int -> Int -> (Int -> Bool) -> (Maybe Int, Maybe Int)
bisect !l !r = runIdentity . bisectM l r . (return .)

-- | \(O(f \log N)\) Also known as lower bound.
{-# INLINE bisectL #-}
bisectL :: Int -> Int -> (Int -> Bool) -> Maybe Int
bisectL !a !b !c = fst $! bisect a b c

-- | \(O(f \log N)\) Also known as upper bound.
{-# INLINE bisectR #-}
bisectR :: Int -> Int -> (Int -> Bool) -> Maybe Int
bisectR !a !b !c = snd $! bisect a b c

-- | \(O(f \log N)\) Monadic binary search.
{-# INLINE bisectM #-}
bisectM :: forall m. (Monad m) => Int -> Int -> (Int -> m Bool) -> m (Maybe Int, Maybe Int)
bisectM !low !high !isOk = both wrap <$> inner (low - 1) (high + 1)
  where
    inner :: Int -> Int -> m (Int, Int)
    inner !ok !ng | abs (ok - ng) == 1 = return (ok, ng)
    inner !ok !ng =
      isOk m >>= \case
        True -> inner m ng
        False -> inner ok m
      where
        !m = (ok + ng) `div` 2

    wrap :: Int -> Maybe Int
    wrap !x
      | inRange (low, high) x = Just x
      | otherwise = Nothing

-- | \(O(f \log N)\)
{-# INLINE bisectML #-}
bisectML :: forall m. (Monad m) => Int -> Int -> (Int -> m Bool) -> m (Maybe Int)
bisectML !a !b !c = fst <$> bisectM a b c

-- | \(O(f \log N)\)
{-# INLINE bisectMR #-}
bisectMR :: forall m. (Monad m) => Int -> Int -> (Int -> m Bool) -> m (Maybe Int)
bisectMR !a !b !c = snd <$> bisectM a b c

-- | \(O(f \log N)\)
{-# INLINE bisectF64 #-}
bisectF64 :: Double -> Double -> Double -> (Double -> Bool) -> (Maybe Double, Maybe Double)
bisectF64 !low !high !diff !isOk = both wrap (inner (low - diff) (high + diff))
  where
    inner :: Double -> Double -> (Double, Double)
    inner !ok !ng | abs (ok - ng) < diff = (ok, ng)
    inner !ok !ng
      | isOk m = inner m ng
      | otherwise = inner ok m
      where
        !m = (ok + ng) / 2
    wrap :: Double -> Maybe Double
    wrap !x
      | x == (low - diff) || x == (high + diff) = Nothing
      | otherwise = Just x

-- | \(O(f \log N)\)
{-# INLINE bisectF64L #-}
bisectF64L :: Double -> Double -> Double -> (Double -> Bool) -> Maybe Double
bisectF64L !a !b !c !d = fst $! bisectF64 a b c d

-- | \(O(f \log N)\)
{-# INLINE bisectF64R #-}
bisectF64R :: Double -> Double -> Double -> (Double -> Bool) -> Maybe Double
bisectF64R !a !b !c !d = snd $! bisectF64 a b c d

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
        then return $ Just i
        else return Nothing
    _ -> return Nothing

-- | Retrieves square root of an `Int`.
isqrtSlow :: Int -> Int
isqrtSlow n = fromJust $ bisectR 0 n ((< n) . (^ (2 :: Int)))
