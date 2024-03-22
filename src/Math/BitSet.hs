-- | Bit set tricks (not so much).
module Math.BitSet where

import Control.Monad
import Data.Bits
import Data.List (unfoldr)
import Data.Tuple.Extra (dupe)
import qualified Data.Vector.Unboxed as U

-- | Retrieves the most significant bit.
--
-- >>> msbOf 0
-- -1
-- >>> msbOf maxBound
-- 62
-- >>> msbOf $ 4 + 2 + 1
-- 2
{-# INLINE msbOf #-}
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
{-# INLINE lsbOf #-}
lsbOf :: Int -> Int
lsbOf = countTrailingZeros

{-# INLINE bitsOf #-}
bitsOf :: Int -> U.Vector Int
bitsOf x0 = U.unfoldrExactN (popCount x0) f x0
  where
    f x =
      let !lsb = countTrailingZeros x
       in (lsb, clearBit x lsb)

-- TODO: super efficient bit operations

-- | Log base of two or bit floor.
-- <https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-Bits.html#v:countLeadingZeros>
{-# INLINE log2 #-}
log2 :: (FiniteBits b) => b -> Int
log2 !x = finiteBitSize x - 1 - countLeadingZeros x

-- | Ceiling of log base 2 of an `Int`.
--
-- = Example
--
-- @
-- > log2 3
-- 1
-- > log2CeilInt 3
-- 2
-- @
{-# INLINE log2CeilInt #-}
log2CeilInt :: Int -> Int
log2CeilInt !x = msb + ceiling_
  where
    !msb = log2 x
    !ceiling_ = if clearBit x msb > 0 then 1 else 0

-- | Calculates the smallest integral power of two that is not smaller than @x@.
--
-- = Example
--
-- @
-- > bitCeil 3
-- 4
-- @
{-# INLINE bitCeil #-}
bitCeil :: Int -> Int
bitCeil = bit . log2CeilInt

-- | \(O(2^N)\) Originally by @yamate11
{-# INLINE powersetM_ #-}
powersetM_ :: (Bits a, Num a, Monad m) => a -> (a -> m ()) -> m ()
powersetM_ !is0 !act = act2 is0
  where
    act2 !is = do
      act is
      unless (is == 0) (act2 (is0 .&. (is - 1)))

-- | \(O(2^N)\) Originally by @yamate11
--
-- >>> powerset (7 :: Int)
-- [7,6,5,4,3,2,1,0]
{-# INLINE powerset #-}
powerset :: (Bits a, Num a) => a -> [a]
powerset !a = a : unfoldr f a
  where
    f 0 = Nothing
    f !x = Just . dupe $! a .&. (x - 1)

-- | \(O(2^N)\) Returns a powerset of @x0@ in descending order.
--
-- >>> powersetU (7 :: Int)
-- [7,6,5,4,3,2,1,0]
{-# INLINE powersetU #-}
powersetU :: (Bits a, Num a, U.Unbox a) => a -> U.Vector a
powersetU !x0 = U.unfoldrExactN n f x0
  where
    !n = bit (popCount x0)
    f !x = (x, (x - 1) .&. x0)

-- | >>> unBitSet 4 5
-- [0,2]
--
-- TODO: which is faster: unfoldrExactN with count leading zeros.
{-# INLINE unBitSet #-}
unBitSet :: Int -> Int -> U.Vector Int
unBitSet n bits = U.filter (testBit bits) (U.generate n id)

-- | \(O(N \cdot N!)\) DFS that enumerates all possible [partitions](https://en.wikipedia.org/wiki/Partition_of_a_set) of a bitset.
-- Prefer `partitionsOfK` when you only need specific size of families.
--
-- >>> partitionsOf 4
-- [[8,4,2,1],[12,2,1],[8,6,1],[4,10,1],[14,1],[8,4,3],[12,3],[8,2,5],[10,5],[8,7],[4,2,9],[6,9],[4,11],[2,13],[15]]
partitionsOf :: Int -> [[Int]]
partitionsOf = inner [] []
  where
    inner :: [[Int]] -> [Int] -> Int -> [[Int]]
    inner !results !acc 0 = acc : results
    inner !results !acc !rest = U.foldl' step results (powersetU rest')
      where
        !lsb = countTrailingZeros rest
        !rest' = clearBit rest lsb
        step !res !set =
          let !set' = set .|. bit lsb
           in inner res (set' : acc) (rest' .&. complement set')

-- | \(O(N \cdot N!)\) (or faster?) DFS that numerates [partitions](https://en.wikipedia.org/wiki/Partition_of_a_set)
-- of size @k0@.
--
-- = Typical problems
-- - [ABC 310 D - Peaceful Teams](https://atcoder.jp/contests/abc310/tasks/abc310_d)
-- - [ABC 319 D - General Weighted Max Matching](https://atcoder.jp/contests/abc318/tasks/abc318_d)
--   (Not the exact pattern though)
-- - [Typical 045 - Simple Grouping (★ 6)](https://atcoder.jp/contests/typical90/tasks/typical90_as)
--
-- = DP
-- Faster solution would be \(O(2^N N)\) (correct?) DP. Use @pushBasedConstructN ((0, 0), (bit n - 1, n))@ or
-- fold-like @times k@ for that. Choose sets with the lsb of the remaining set only.
partitionsOfK :: Int -> Int -> [[Int]]
partitionsOfK set0 k0 = inner [] k0 [] set0
  where
    inner :: [[Int]] -> Int -> [Int] -> Int -> [[Int]]
    -- inner results (-1) _ _ = results
    inner !results 0 acc 0 = acc : results
    inner !results 0 _ _ = results
    inner !results !k !acc !rest
      | k > popCount rest = results
      | otherwise = U.foldl' step results (powersetU rest')
      where
        !lsb = countTrailingZeros rest
        !rest' = clearBit rest lsb
        step !res !set =
          let !set' = set .|. bit lsb
           in inner res (k - 1) (set' : acc) (rest' .&. complement set')

