{-# LANGUAGE TypeFamilies #-}

module Data.SafeList where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG

-- | Safelist
class SafeList v where
  type SafeListElem v

  headMay :: v -> Maybe (SafeListElem v)
  lastMay :: v -> Maybe (SafeListElem v)

  headOr :: SafeListElem v -> v -> SafeListElem v
  lastOr :: SafeListElem v -> v -> SafeListElem v

  minimumMay :: v -> Maybe (SafeListElem v)
  maximumMay :: v -> Maybe (SafeListElem v)

  minimumOr :: SafeListElem v -> v -> SafeListElem v
  maximumOr :: SafeListElem v -> v -> SafeListElem v

-- | = Tests
--
-- >>> headMay ([] :: [Int])
-- Nothing
-- >>> headMay ([1, 2, 3] :: [Int])
-- Just 1
--
-- >>> lastMay ([] :: [Int])
-- Nothing
-- >>> lastMay ([1, 2, 3] :: [Int])
-- Just 3
--
-- >>> headOr (-1) ([] :: [Int])
-- -1
-- >>> headOr (-1) ([1, 2, 3] :: [Int])
-- 1
--
-- >>> lastOr (-1) ([] :: [Int])
-- -1
-- >>> lastOr (-1) ([1, 2, 3] :: [Int])
-- 3
--
-- >>> minimumMay ([] :: [Int])
-- Nothing
-- >>> minimumMay ([2, 1, 4, 3] :: [Int])
-- Just 1
--
-- >>> maximumMay ([] :: [Int])
-- Nothing
-- >>> maximumMay ([2, 1, 4, 3] :: [Int])
-- Just 4
--
-- >>> minimumOr (-1) ([] :: [Int])
-- -1
-- >>> minimumOr (-1) ([2, 1, 4, 3] :: [Int])
-- 1
--
-- >>> maximumOr (-1) ([] :: [Int])
-- -1
-- >>> maximumOr (-1) ([2, 1, 4, 3] :: [Int])
-- 4
instance (Ord a) => SafeList [a] where
  type SafeListElem [a] = a

  headMay [] = Nothing
  headMay (x:_) = Just x

  lastMay [] = Nothing
  lastMay xs = Just $ last xs

  headOr x0 [] = x0
  headOr _ xs = head xs

  lastOr x0 [] = x0
  lastOr _ xs = last xs

  minimumMay [] = Nothing
  minimumMay xs = Just $ minimum xs

  maximumMay [] = Nothing
  maximumMay xs = Just $ maximum xs

  minimumOr x0 [] = x0
  minimumOr _ xs = minimum xs

  maximumOr x0 [] = x0
  maximumOr _ xs = maximum xs

-- | = Tests
--
-- >>> headMay $ V.fromList ([] :: [Int])
-- Nothing
-- >>> headMay $ V.fromList ([1, 2, 3] :: [Int])
-- Just 1
--
-- >>> lastMay $ V.fromList ([] :: [Int])
-- Nothing
-- >>> lastMay $ V.fromList ([1, 2, 3] :: [Int])
-- Just 3
--
-- >>> headOr (-1) $ V.fromList ([] :: [Int])
-- -1
-- >>> headOr (-1) $ V.fromList ([1, 2, 3] :: [Int])
-- 1
--
-- >>> lastOr (-1) $ V.fromList ([] :: [Int])
-- -1
-- >>> lastOr (-1) $ V.fromList ([1, 2, 3] :: [Int])
-- 3
--
-- >>> minimumMay $ V.fromList ([] :: [Int])
-- Nothing
-- >>> minimumMay $ V.fromList ([2, 1, 4, 3] :: [Int])
-- Just 1
--
-- >>> maximumMay $ V.fromList ([] :: [Int])
-- Nothing
-- >>> maximumMay $ V.fromList ([2, 1, 4, 3] :: [Int])
-- Just 4
--
-- >>> minimumOr (-1) $ V.fromList ([] :: [Int])
-- -1
-- >>> minimumOr (-1) $ V.fromList ([2, 1, 4, 3] :: [Int])
-- 1
--
-- >>> maximumOr (-1) $ V.fromList ([] :: [Int])
-- -1
-- >>> maximumOr (-1) $ V.fromList ([2, 1, 4, 3] :: [Int])
-- 4
instance (Ord a) => SafeList (V.Vector a) where
  type SafeListElem (V.Vector a) = a

  headMay xs
    | VG.null xs = Nothing
    | otherwise = Just $ VG.unsafeHead xs

  lastMay xs
    | VG.null xs = Nothing
    | otherwise = Just $ VG.unsafeLast xs

  headOr x0 xs
    | VG.null xs = x0
    | otherwise = VG.unsafeHead xs

  lastOr x0 xs
    | VG.null xs = x0
    | otherwise = VG.unsafeLast xs

  minimumMay xs
    | VG.null xs = Nothing
    | otherwise = Just $ VG.minimum xs

  maximumMay xs
    | VG.null xs = Nothing
    | otherwise = Just $ VG.maximum xs

  minimumOr x0 xs
    | VG.null xs = x0
    | otherwise = VG.minimum xs

  maximumOr x0 xs
    | VG.null xs = x0
    | otherwise = VG.maximum xs

-- | The implementation is same as the one for `V.Vector`.
instance (VU.Unbox a, Ord a) => SafeList (VU.Vector a) where
  type SafeListElem (VU.Vector a) = a

  headMay xs
    | VG.null xs = Nothing
    | otherwise = Just $ VG.unsafeHead xs

  lastMay xs
    | VG.null xs = Nothing
    | otherwise = Just $ VG.unsafeLast xs

  headOr x0 xs
    | VG.null xs = x0
    | otherwise = VG.unsafeHead xs

  lastOr x0 xs
    | VG.null xs = x0
    | otherwise = VG.unsafeLast xs

  minimumMay xs
    | VG.null xs = Nothing
    | otherwise = Just $ VG.minimum xs

  maximumMay xs
    | VG.null xs = Nothing
    | otherwise = Just $ VG.maximum xs

  minimumOr x0 xs
    | VG.null xs = x0
    | otherwise = VG.minimum xs

  maximumOr x0 xs
    | VG.null xs = x0
    | otherwise = VG.maximum xs

