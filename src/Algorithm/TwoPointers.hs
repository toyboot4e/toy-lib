-- | Two pointers
module Algorithm.TwoPointers where

import Data.Bifunctor (second)

-- | Returns inclusive ranges that satisfy the given `check`.
-- FIXME: Use a simpler, cheaper implementation
twoPointers :: Int -> ((Int, Int) -> Bool) -> [(Int, Int)]
twoPointers !n !check = inner (0, 0)
  where
    inner (!l, !_) | l >= n = []
    inner (!l, !r)
      | check (l, r) =
          let (!l', !r') = until (not . peekCheck) (second succ) (l, r)
           in (l', r') : inner (succ l', max l' r')
      | otherwise = inner (succ l, max (succ l) r)
    peekCheck (!_, !r) | r == pred n = False
    peekCheck (!l, !r) = check (l, succ r)

-- TODO: take both left and right and use one for popping
-- TODO: vector version
