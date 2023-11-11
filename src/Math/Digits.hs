-- | Obsolute. TODO: Delete.
module Math.Digits where

import Data.List (foldl', genericTake)
import Data.Maybe

-- TODO: revive

-- {{{ Digits

-- Taken from <https://hackage.haskell.org/package/digits-0.3.1/docs/Data-Digits.html>

-- digitToInt :: Char -> Int

-- | Returns the digits of a positive integer as a Maybe list, in reverse order or Nothing if a zero
-- or negative base is given. This is slightly more efficient than in forward order.
mDigitsRev :: (Integral n) => n -> n -> Maybe [n]
mDigitsRev !base !i = if base < 1 then Nothing else Just $ dr base i
  where
    dr _ 0 = []
    dr !b !x = case base of
      1 -> genericTake x $ repeat 1
      _ ->
        let (!rest, !lastDigit) = quotRem x b
         in lastDigit : dr b rest

-- | Returns the digits of a positive integer as a Maybe list.
-- or Nothing if a zero or negative base is given
mDigits :: (Integral n) => n -> n -> Maybe [n]
mDigits !base !i = reverse <$> mDigitsRev base i

-- | Returns the digits of a positive integer as a list, in reverse order.
-- Throws an error if given a zero or negative base.
digitsRev :: (Integral n) => n -> n -> [n]
digitsRev !base = fromJust . mDigitsRev base

-- | Returns the digits of a positive integer as a list.
-- REMARK: It's modified to return `[0]` when given zero.
digits :: (Integral n) => n -> n -> [n]
digits _ 0 = [0]
digits !base !x = reverse $ digitsRev base x

-- | Takes a list of digits, and converts them back into a positive integer.
unDigits :: (Integral n) => n -> [n] -> n
unDigits !base = foldl' (\ !a !b -> a * base + b) 0

-- | <https://stackoverflow.com/questions/10028213/converting-number-base>
-- REMARK: It returns `[]` when given `[0]`. Be sure to convert `[]` to `[0]` if necessary.
convertBase :: (Integral a) => a -> a -> [a] -> [a]
convertBase !from !to = digits to . unDigits from

-- }}}
