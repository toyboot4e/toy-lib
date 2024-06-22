module Math.FloorSum where

import Data.Bool (bool)

-- | \(O(\log ma)\) Floor sum: <https://atcoder.jp/contests/practice2/tasks/practice2_c>
--
-- = Input constraints
-- - n > 0
-- - m >= 0
-- - a >= 0
-- - b >= 0
floorSumUnsigned :: Int -> Int -> Int -> Int -> Int
floorSumUnsigned = inner
  where
    inner _ 0 _ _ = 0
    inner n m a b = n1 + n2 + n3
      where
        (!qa, !ra) = a `divMod` m
        (!qb, !rb) = b `divMod` m
        n1 = n * qb
        n2 = qa * (n - 1) * n `div` 2
        (!qy, !ry) = (ra * n + rb) `divMod` m
        n3
          | qy < 1 = 0
          | otherwise = inner qy ra m ry

-- | \(O(\log ma)\) Floor sum: <https://atcoder.jp/contests/practice2/tasks/practice2_c>
--
-- = Input constraints
-- - n > 0
-- - m >= 0
floorSum :: Int -> Int -> Int -> Int -> Int
floorSum n m a b = n1 + n2 + floorSumUnsigned n m a' b'
  where
    a' = bool a (a `mod` m) $ a < 0
    n2 = bool 0 (a `div` m * (n - 1) * n `div` 2) $ b < 0
    b' = bool b (b `mod` m) $ b < 0
    n1 = bool 0 (n * (b `div` m)) $ a < 0
