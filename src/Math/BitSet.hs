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

-- | Originally by @yamate11
{-# INLINE powersetM_ #-}
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
{-# INLINE powerset #-}
powerset :: (Bits a, Num a) => a -> [a]
powerset !a = a : unfoldr f a
  where
    f 0 = Nothing
    f !x = Just . dupe $! a .&. (x - 1)

-- | Returns a powerset of @x0@ in descending order.
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
