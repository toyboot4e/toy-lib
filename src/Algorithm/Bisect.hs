{-# LANGUAGE LambdaCase #-}

-- | \(O(log N)\) binary search for sorted items in an inclusive range (from left to right only).
--
-- `bisect` returns an @(ok, ng)@ index pair at the boundary. `bsearchL` and `bsearchR` returns
-- one of the pair. `bsearchM` is a monadic variant of `bisect`.
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
-- `bsearchL` returns @Just 5@ and `bsearchR` returns @Just 6@.
module Algorithm.Bisect where

import Data.Functor.Identity
import Data.Ix
import Data.Maybe
import Data.Tuple.Extra hiding (first, second)
import qualified Data.Vector.Generic as G
import ToyLib.Prelude

-- TODO: Use typeclass for getting middle and detecting end

-- | `bisect` over a vector.
{-# INLINE bsearch #-}
bsearch :: (G.Vector v a) => v a -> (a -> Bool) -> (Maybe Int, Maybe Int)
bsearch !vec !p = bisect (0, G.length vec - 1) (p . (vec G.!))

-- | `bisectL` over a vector.
{-# INLINE bsearchL #-}
bsearchL :: (G.Vector v a) => v a -> (a -> Bool) -> Maybe Int
bsearchL !vec !p = bisectL (0, G.length vec - 1) (p . (vec G.!))

-- | `bisectR` over a vector.
{-# INLINE bsearchR #-}
bsearchR :: (G.Vector v a) => v a -> (a -> Bool) -> Maybe Int
bsearchR !vec !p = bisectR (0, G.length vec - 1) (p . (vec G.!))

-- | `bsearchL` over a vector, searching for a specific value. FIXME: It's slower than `bsearchLG`.
{-# INLINE bsearchExact #-}
bsearchExact :: (G.Vector v a, Ord b) => v a -> (a -> b) -> b -> Maybe Int
bsearchExact !vec f !xref = case bisectL (0, G.length vec - 1) ((<= xref) . f . (vec G.!)) of
  Just !x | f (vec G.! x) == xref -> Just x
  _ -> Nothing

-- | Pure binary search.
{-# INLINE bisect #-}
bisect :: (Int, Int) -> (Int -> Bool) -> (Maybe Int, Maybe Int)
bisect !rng = runIdentity . bisectM rng . (return .)

-- | Also known as lower bound.
{-# INLINE bisectL #-}
bisectL :: (Int, Int) -> (Int -> Bool) -> Maybe Int
bisectL !a !b = fst $! bisect a b

-- | Also known as upper bound.
{-# INLINE bisectR #-}
bisectR :: (Int, Int) -> (Int -> Bool) -> Maybe Int
bisectR !a !b = snd $! bisect a b

-- | Monadic binary search.
{-# INLINE bisectM #-}
bisectM :: forall m. (Monad m) => (Int, Int) -> (Int -> m Bool) -> m (Maybe Int, Maybe Int)
bisectM (!low, !high) !isOk = both wrap <$> inner (low - 1, high + 1)
  where
    inner :: (Int, Int) -> m (Int, Int)
    inner (!ok, !ng) | abs (ok - ng) == 1 = return (ok, ng)
    inner (!ok, !ng) =
      isOk m >>= \case
        True -> inner (m, ng)
        False -> inner (ok, m)
      where
        !m = (ok + ng) `div` 2

    wrap :: Int -> Maybe Int
    wrap !x
      | inRange (low, high) x = Just x
      | otherwise = Nothing

{-# INLINE bisectML #-}
bisectML :: forall m. (Monad m) => (Int, Int) -> (Int -> m Bool) -> m (Maybe Int)
bisectML = fmap fst .: bisectM

{-# INLINE bisectRM #-}
bisectRM :: forall m. (Monad m) => (Int, Int) -> (Int -> m Bool) -> m (Maybe Int)
bisectRM = fmap snd .: bisectM

{-# INLINE bisectF32 #-}
bisectF32 :: (Float, Float) -> Float -> (Float -> Bool) -> (Maybe Float, Maybe Float)
bisectF32 (!low, !high) !diff !isOk = both wrap (inner (low - diff, high + diff))
  where
    inner :: (Float, Float) -> (Float, Float)
    inner (!ok, !ng) | abs (ok - ng) <= diff = (ok, ng)
    inner (!ok, !ng)
      | isOk m = inner (m, ng)
      | otherwise = inner (ok, m)
      where
        !m = (ok + ng) / 2
    wrap :: Float -> Maybe Float
    wrap !x
      | x == (low - diff) || x == (high + diff) = Nothing
      | otherwise = Just x

{-# INLINE bisectF32L #-}
bisectF32L :: (Float, Float) -> Float -> (Float -> Bool) -> Maybe Float
bisectF32L !a !b !c = fst $! bisectF32 a b c

{-# INLINE bisectF32R #-}
bisectF32R :: (Float, Float) -> Float -> (Float -> Bool) -> Maybe Float
bisectF32R !a !b !c = fst $! bisectF32 a b c

{-# INLINE bisectF64 #-}
bisectF64 :: (Double, Double) -> Double -> (Double -> Bool) -> (Maybe Double, Maybe Double)
bisectF64 (!low, !high) !diff !isOk = both wrap (inner (low - diff, high + diff))
  where
    inner :: (Double, Double) -> (Double, Double)
    inner (!ok, !ng) | abs (ok - ng) < diff = (ok, ng)
    inner (!ok, !ng)
      | isOk m = inner (m, ng)
      | otherwise = inner (ok, m)
      where
        !m = (ok + ng) / 2
    wrap :: Double -> Maybe Double
    wrap !x
      | x == (low - diff) || x == (high + diff) = Nothing
      | otherwise = Just x

{-# INLINE bisectF64L #-}
bisectF64L :: (Double, Double) -> Double -> (Double -> Bool) -> Maybe Double
bisectF64L !a !b !c = fst $! bisectF64 a b c

{-# INLINE bisectF64R #-}
bisectF64R :: (Double, Double) -> Double -> (Double -> Bool) -> Maybe Double
bisectF64R !a !b !c = fst $! bisectF64 a b c

-- | Retrieves square root of an `Int`.
isqrtSlow :: Int -> Int
isqrtSlow n = fromJust $ bisectR (0, n) ((< n) . (^ (2 :: Int)))
