{-# LANGUAGE BangPatterns #-}

-- | The number of divisor is smaller than \(log N\).
-- See also: <https://scrapbox.io/magurofly/%E7%B4%84%E6%95%B0%E3%81%AE%E5%80%8B%E6%95%B0%E3%81%AE%E3%82%AA%E3%83%BC%E3%83%80%E3%83%BC>
--
-- = Tips
-- - The last number could be decided with division, not enumeration.
--
-- = Typical problems
-- - [Typical 085 - Multiplication (★4)](https://atcoder.jp/contests/typical90/tasks/typical90_cg)
-- - [ABC 300 D - AABBC (Difficulty 908)](https://atcoder.jp/contests/abc300/tasks/abc300_d)
--
-- = Prime factors
-- - [Typical 030 - K Factors (★5)](https://atcoder.jp/contests/typical90/tasks/typical90_ad)
--   Numbers with i >= k prime factors can be enumerated very fastly.
--
-- = Other problems
-- - [ABC 254 D - Together Square (Difficulty 1191)](https://atcoder.jp/contests/abc254/tasks/abc254_d)
module Math.Divisors where

import Data.List

-- \(O(log N)\) divisor enumeration (sorted).
divisorsOf :: Int -> [Int]
divisorsOf n = sort $ inner 1
  where
    inner k
      -- no dependency to `Int` square root function
      | k * k > n = []
      -- no divisor duplication
      | k * k == n = [k]
      -- not sorted yet
      | r == 0 = k : d : inner (succ k)
      -- ignore non divisors
      | otherwise = inner (succ k)
      where
        -- This strict evaluation and unboxing takes some effect, even though they're not always
        -- used.
        (!d, !r) = n `divMod` k
