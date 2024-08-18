-- | Prime number enumeration and prime factorization.
module Math.Stimes where

import Data.Bits

-- | \(O(W)\) Strict, much faster `stimes`.
{-# INLINE stimes' #-}
stimes' :: (Semigroup a) => Int -> a -> a
stimes' n0 x1
  | n0 <= 0 = errorWithoutStackTrace "stimes: positive multiplier expected"
  | otherwise = power n0 (<>) x1

-- | \(O(W)\) Strict, much faster `mtimes`.
{-# INLINE mtimes' #-}
mtimes' :: (Monoid a) => Int -> a -> a
mtimes' n0 x1 = case compare n0 0 of
  LT -> errorWithoutStackTrace "mtimes: zero or positive multiplier expected"
  EQ -> mempty
  GT -> power n0 (<>) x1

-- | \(O(W)\) Calculates @s^n@ by @n@ (N > 0) times using the binary lifting technique.
{-# INLINE power #-}
power :: Int -> (a -> a -> a) -> a -> a
power n0 op x1
  | n0 <= 0 = errorWithoutStackTrace "power: positive multiplier expected"
  | otherwise = f x1 n0
  where
    f !x !n
      | even n = f (x `op` x) (n .>>. 1)
      | n == 1 = x
      | otherwise = g (x `op` x) (n .>>. 1) x
    g !x !n !z
      | even n = g (x `op` x) (n .>>. 1) z
      | n == 1 = x `op` z
      | otherwise = g (x `op` x) (n .>>. 1) (x `op` z)

-- | \(O(W)\) Calculates @s^n@ by @n@ (N > 0) times using the binary lifting technique.
{-# INLINE powerOr #-}
powerOr :: Int -> (a -> a -> a) -> a -> a -> a
powerOr n0 op x1 x0
  | n0 <= 0 = x0
  | otherwise = power n0 op x1

-- | \(O(W)\) Multiplies @x by @n@ times using the binary lifting technique.
{-# INLINE powerMay #-}
powerMay :: Int -> (a -> a -> a) -> a -> Maybe a
powerMay n0 op x1
  | n0 <= 0 = Nothing
  | otherwise = Just $! power n0 op x1
