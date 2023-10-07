-- | Digits by `Vector`
module Math.DigitsVector where

import Data.Tuple.Extra hiding (first, second)
import qualified Data.Vector.Unboxed as VU

-- | >>> toDigitsVU 3 ((3^0)*1 + (3^1)*2 + (3^2)*1)
-- [1,2,1]
toDigitsVU :: Int -> Int -> VU.Vector Int
toDigitsVU !base !x0
  | x0 < base = VU.singleton x0
  | otherwise = VU.unfoldr expand x0
  where
    expand 0 = Nothing
    expand x = Just $ swap (x `divMod` base)

-- | >>> unDigitsVU 3 (VU.fromList [1,2,1]) == ((3^0)*1 + (3^1)*2 + (3^2)*1)
-- True
-- >>> unDigitsVU 3 (toDigitsVU 3 ((3^0)*1 + (3^1)*2 + (3^2)*1)) == ((3^0)*1 + (3^1)*2 + (3^2)*1)
-- True
unDigitsVU :: Int -> VU.Vector Int -> Int
unDigitsVU !base !xs = fst $ VU.foldl' step (0 :: Int, 1 :: Int) xs
  where
    step (!acc, !d) !i = (acc + d * i, base * d)

