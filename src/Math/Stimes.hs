-- | Prime number enumeration and prime factorization.
module Math.Stimes where

import Data.Bits

-- | Strict, much faster `stimes`.
{-# INLINE stimes' #-}
stimes' :: (Semigroup a) => Int -> a -> a
stimes' n0 x0
  | n0 <= 0 = errorWithoutStackTrace "stimes: positive multiplier expected"
  | otherwise = mulTimes n0 (<>) x0

-- | Strict, much faster `mtimes`.
{-# INLINE mtimes' #-}
mtimes' :: (Monoid a) => Int -> a -> a
mtimes' n0 x0 = case compare n0 0 of
  LT -> errorWithoutStackTrace "mtimes: zero or positive multiplier expected"
  EQ -> mempty
  GT -> mulTimes n0 (<>) x0

-- | Multiplies @x@ by @n@ (N > 0) times using the binary lifting technique.
{-# INLINE mulTimes #-}
mulTimes :: Int -> (a -> a -> a) -> a -> a
mulTimes n0 op x0
  | n0 <= 0 = errorWithoutStackTrace "mulTimes: positive multiplier expected"
  | otherwise = f x0 n0
  where
    f !x !n
      | even n = f (x `op` x) (n .>>. 1)
      | n == 1 = x
      | otherwise = g (x `op` x) (n .>>. 1) x
    g !x !n !z
      | even n = g (x `op` x) (n .>>. 1) z
      | n == 1 = x `op` z
      | otherwise = g (x `op` x) (n .>>. 1) (x `op` z)

-- | Multiplies @x by @n@ times using the binary lifting technique.
{-# INLINE mulTimesMay #-}
mulTimesMay :: Int -> (a -> a -> a) -> a -> Maybe a
mulTimesMay n0 op x0
  | n0 <= 0 = Nothing
  | otherwise = Just $! mulTimes n0 op x0
