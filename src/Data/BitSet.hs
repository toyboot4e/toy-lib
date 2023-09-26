-- | Bit set tricks
module Data.BitSet where

import Control.Monad
import Data.Bits
import Data.List (unfoldr)
import Data.Tuple.Extra (dupe)
import qualified Data.Vector.Unboxed as VU

-- | Retrieves the most significant bit.
--
-- >>> msbOf 0
-- -1
-- >>> msbOf maxBound
-- 62
-- >>> msbOf $ 4 + 2 + 1
-- 2
msbOf :: Int -> Int
msbOf !x = 63 - countLeadingZeros x

-- | Retrieves the least significant bit.
--
-- >>> lsbOf 0
-- 64
-- >>> lsbOf maxBound
-- 0
-- >>> lsbOf $ 4 + 2 + 1
-- 0
lsbOf :: Int -> Int
lsbOf = countTrailingZeros

-- | Originally by @yamate11
powersetM_ :: (Bits a, Num a, Monad m) => a -> (a -> m ()) -> m ()
powersetM_ !is0 !act = act2 is0
  where
    act2 !is = do
      act is
      unless (is == 0) (act2 (is0 .&. (is - 1)))

-- | Originally by @yamate11
--
-- >>> powerset (7 :: Int)
-- [7,6,5,4,3,2,1,0]
powerset :: (Bits a, Num a) => a -> [a]
powerset !a = a : unfoldr f a
  where
    f 0 = Nothing
    f !x = Just . dupe $! a .&. (x - 1)

-- | Returns a powerset of @x0@ in descending order.
--
-- >>> powersetVU (7 :: Int)
-- [7,6,5,4,3,2,1,0]
powersetVU :: (Bits a, Num a, VU.Unbox a) => a -> VU.Vector a
powersetVU !x0 = VU.unfoldrExactN n f x0
  where
    !n = bit (popCount x0)
    f !x = (x, (x - 1) .&. x0)
