-- | Obsolute. TODO: Delete.
module Math.Bits where

import Data.Bits

-- {{{ Bits

-- TODO: super efficient bit operations

-- | Log base of two or bit floor.
-- <https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-Bits.html#v:countLeadingZeros>
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
bitCeil :: Int -> Int
bitCeil = bit . log2CeilInt

-- }}}
