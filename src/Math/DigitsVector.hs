-- | Digits by `Vector`
module Math.DigitsVector where

import Data.Tuple.Extra hiding (first, second)
import qualified Data.Vector.Unboxed as U

-- | >>> toDigitsU 3 ((3^0)*1 + (3^1)*2 + (3^2)*1)
-- [1,2,1]
toDigitsU :: Int -> Int -> U.Vector Int
toDigitsU !base !x0
  | x0 < base = U.singleton x0
  | otherwise = U.unfoldr expand x0
  where
    expand 0 = Nothing
    expand x = Just $ swap (x `divMod` base)

-- | >>> toNDigitsU 3 3 ((3^0)*1 + (3^1)*0 + (3^2)*0)
-- [1,0,0]
toNDigitsU :: Int -> Int -> Int -> U.Vector Int
toNDigitsU !base !nDigits !x0 = U.unfoldrExactN nDigits expand x0
  where
    expand x = swap (x `divMod` base)

-- | >>> unDigitsU 3 (U.fromList [1,2,1]) == ((3^0)*1 + (3^1)*2 + (3^2)*1)
-- True
-- >>> unDigitsU 3 (toDigitsU 3 ((3^0)*1 + (3^1)*2 + (3^2)*1)) == ((3^0)*1 + (3^1)*2 + (3^2)*1)
-- True
unDigitsU :: Int -> U.Vector Int -> Int
unDigitsU !base !xs = fst $ U.foldl' step (0 :: Int, 1 :: Int) xs
  where
    step (!acc, !d) !i = (acc + d * i, base * d)

