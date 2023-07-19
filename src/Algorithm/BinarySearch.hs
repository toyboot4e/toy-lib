{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | \(O(log N)\) binary search for sorted items in an inclusive range (from left to right only).
--
-- `bsearch` returns an @(ok, ng)@ index pair at the boundary. `bsearchL` and `bsearchR` returns
-- one of the pair. `bsearchM` is a monadic variant of `bsearch`.
--
-- = Example
--
-- With an OK predicate @(<= 5)@, list @[0..9]@ can be seen as:
--
-- > [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
-- >  <-------------->  <-------->
-- >         ok             ng
--
-- In the preceding example, `bsearch` returns the @(ok, ng)@ = @(Just 5, Just 6)@ pair at the boundary:
--
-- >>> :{
-- let xs = [0 :: Int .. 9]
--  in bsearch (0 :: Int, 9 :: Int) (\i -> xs !! i <= 5)
-- :}
-- (Just 5,Just 6)
--
-- `bsearchL` returns @Just 5@ and `bsearchR` returns @Just 6@.
module Algorithm.BinarySearch where

import Data.Functor.Identity
import Data.Ix
import Data.List.Extra (nubSort)
import Data.Maybe
import Data.Tuple.Extra hiding (first, second)
import qualified Data.Vector.Unboxed as VU
import ToyLib.Prelude

-- TODO: Use typeclass for getting middle and detecting end

-- | Pure binary search.
{-# INLINE bsearch #-}
bsearch :: (Int, Int) -> (Int -> Bool) -> (Maybe Int, Maybe Int)
bsearch !rng = runIdentity . bsearchM rng . (return .)

-- | Also known as lower bound.
{-# INLINE bsearchL #-}
bsearchL :: (Int, Int) -> (Int -> Bool) -> Maybe Int
bsearchL !a !b = fst $! bsearch a b

-- | Also known as upper bound.
{-# INLINE bsearchR #-}
bsearchR :: (Int, Int) -> (Int -> Bool) -> Maybe Int
bsearchR !a !b = snd $! bsearch a b

-- | Monadic binary search.
{-# INLINE bsearchM #-}
bsearchM :: forall m. (Monad m) => (Int, Int) -> (Int -> m Bool) -> m (Maybe Int, Maybe Int)
bsearchM (!low, !high) !isOk = both wrap <$> inner (low - 1, high + 1)
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

{-# INLINE bsearchML #-}
bsearchML :: forall m. (Monad m) => (Int, Int) -> (Int -> m Bool) -> m (Maybe Int)
bsearchML = fmap fst .: bsearchM

{-# INLINE bsearchMR #-}
bsearchMR :: forall m. (Monad m) => (Int, Int) -> (Int -> m Bool) -> m (Maybe Int)
bsearchMR = fmap snd .: bsearchM

{-# INLINE bsearchF32 #-}
bsearchF32 :: (Float, Float) -> Float -> (Float -> Bool) -> (Maybe Float, Maybe Float)
bsearchF32 (!low, !high) !diff !isOk = both wrap (inner (low - diff, high + diff))
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

{-# INLINE bsearchF32L #-}
bsearchF32L :: (Float, Float) -> Float -> (Float -> Bool) -> Maybe Float
bsearchF32L !a !b !c = fst $! bsearchF32 a b c

{-# INLINE bsearchF32R #-}
bsearchF32R :: (Float, Float) -> Float -> (Float -> Bool) -> Maybe Float
bsearchF32R !a !b !c = fst $! bsearchF32 a b c

{-# INLINE bsearchF64 #-}
bsearchF64 :: (Double, Double) -> Double -> (Double -> Bool) -> (Maybe Double, Maybe Double)
bsearchF64 (!low, !high) !diff !isOk = both wrap (inner (low - diff, high + diff))
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

{-# INLINE bsearchF64L #-}
bsearchF64L :: (Double, Double) -> Double -> (Double -> Bool) -> Maybe Double
bsearchF64L !a !b !c = fst $! bsearchF64 a b c

{-# INLINE bsearchF64R #-}
bsearchF64R :: (Double, Double) -> Double -> (Double -> Bool) -> Maybe Double
bsearchF64R !a !b !c = fst $! bsearchF64 a b c

-- | One dimensional index compression: xs -> (nubSorted, indices)
compressList :: [Int] -> (VU.Vector Int, [Int])
compressList xs = (indices, map (fromJust . fst . f) xs)
  where
    !indices = VU.fromList $ nubSort xs
    f !x = bsearch (0, pred (vLength indices)) $ \i -> indices VU.! i <= x

-- | Retrieves square root of an `Int`.
isqrtSlow :: Int -> Int
isqrtSlow n = fromJust $ bsearchR (0, n) ((< n) . (^ (2 :: Int)))
