{-# LANGUAGE BangPatterns #-}

-- | Prime number enumeration and prime factorization.

module Math.Primes where

import Data.List (group)

-- {{{ Prime factors

-- -- @gotoki_no_joe
-- primes :: [Int]
-- primes = 2 : 3 : sieve q0 [5, 7 ..]
--   where
--     q0 = H.insert (H.Entry 9 6) H.empty
--     sieve queue xxs@(x : xs) =
--       case compare np x of
--         LT -> sieve queue1 xxs
--         EQ -> sieve queue1 xs
--         GT -> x : sieve queue2 xs
--       where
--         H.Entry np p2 = H.minimum queue
--         queue1 = H.insert (H.Entry (np + p2) p2) $ H.deleteMin queue
--         queue2 = H.insert (H.Entry (x * x) (x * 2)) queue
--     sieve _ _ = error "unreachale"

-- | @0xYusuke
-- <https://zenn.dev/link/comments/1022553732563c>
primes :: [Int]
primes = 2 : 3 : minus [5, 7 ..] (unionAll [[p * p, p * p + 2 * p ..] | p <- tail primes])
  where
    minus (x : xs) (y : ys) = case (compare x y) of
      LT -> x : minus xs (y : ys)
      EQ -> minus xs ys
      GT -> minus (x : xs) ys
    minus xs _ = xs

    union (x : xs) (y : ys) = case (compare x y) of
      LT -> x : union xs (y : ys)
      EQ -> x : union xs ys
      GT -> y : union (x : xs) ys
    union xs [] = xs
    union [] ys = ys

    unionAll :: Ord a => [[a]] -> [a]
    unionAll ((x : xs) : t) = x : union xs (unionAll $ pairs t)
      where
        pairs ((x : xs) : ys : t) = (x : union xs ys) : pairs t
    unionAll _ = error "unionAll: unreachable"

-- | Returns @[(prime, count)]@
primeFactors :: Int -> [(Int, Int)]
primeFactors !n_ = map (\ !xs -> (head xs, length xs)) . group $ inner n_ input
  where
    -- TODO: reuse `primes`?
    input = 2 : 3 : [y | x <- [5, 11 ..], y <- [x, x + 2]]
    inner n pps@(p : ps)
      | n == 1 = []
      | n < p * p = [n]
      | r == 0 = p : inner q pps
      | otherwise = inner n ps
      where
        (q, r) = divMod n p
    inner _ _ = error "unreachable"

-- }}}
