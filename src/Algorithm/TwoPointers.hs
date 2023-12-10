-- | Two pointers method
module Algorithm.TwoPointers where

import Data.List (unfoldr)
import qualified Data.Vector.Unboxed as U

-- | Returns inclusive ranges that satisfy the given `check`.
twoPointers :: Int -> (Int -> Int -> Bool) -> [(Int, Int)]
twoPointers !n !p = unfoldr (uncurry f) s0
  where
    -- the inners are the same as @twoPointersU@:
    !s0 = (0, 0) :: (Int, Int)
    f l r
      | l == n = Nothing
      | not (p l r) = f (l + 1) (max (l + 1) r)
      | otherwise = Just ((l, r'), (l + 1, max (l + 1) r'))
        where
          -- run peek check and advance on success
          r' = until ((||) <$> (== n - 1) <*> not . p l . succ) succ r

-- | Returns inclusive ranges that satisfy the given `check`.
twoPointersU :: Int -> (Int -> Int -> Bool) -> U.Vector (Int, Int)
twoPointersU !n !p = U.unfoldr (uncurry f) s0
  where
    !s0 = (0, 0) :: (Int, Int)
    f l r
      | l == n = Nothing
      | not (p l r) = f (l + 1) (max (l + 1) r)
      | otherwise = Just ((l, r'), (l + 1, max (l + 1) r'))
        where
          -- run peek check and advance on success
          r' = until ((||) <$> (== n - 1) <*> not . p l . succ) succ r

