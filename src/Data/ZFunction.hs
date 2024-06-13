{-# LANGUAGE RecordWildCards #-}

-- | [Z function](https://cp-algorithms.com/string/z-function.html) calculation.
module Data.ZFunction where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- | \(O(\max(N, M))\) Longest common prefix calculation.
lcpOf :: BS.ByteString -> BS.ByteString -> Int
lcpOf bs1 bs2 = length . takeWhile id $ BS.zipWith (==) bs1 bs2

-- | \(O(N^2)\) Z function calculation.
zOfNaive :: BS.ByteString -> U.Vector Int
zOfNaive bs = U.generate (BS.length bs) z
  where
    z 0 = 0
    z i = lcpOf bs (BS.drop i bs)

-- | \(O(N)\) Z function calculation.
--
-- = The \(O(N)\) algorithm
--
-- Here is the visualization of intermediate calculation:
--
-- @
-- String: [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
--          *-----+-----o
--          0           r' = r0 - l0 + 1
--                i' = i - l
--  Z-Box:        *-----+--------o
--                l0          r0
--                      i
--  Next Z-Box:         *--+---][-+----o
--                         |      |
--                         |      Known by new search
--                         +-- Known by z[i']
-- @
--
-- z[i] can be calculated utilizing z[i'] because s[i..r] = s[i' .. r']
zOf :: BS.ByteString -> U.Vector Int
zOf bs
  | BS.null bs = U.empty
-- kinda constructN, but manually with states
zOf bs = U.create $ do
  let !n = BS.length bs
  z <- UM.unsafeNew n
  UM.unsafeWrite z 0 0

  let calc l0 r0 i
        | i >= n = return ()
        -- starting with unknown point
        | r0 <= i = do
            let !r = lcpSearch 0 i
            UM.unsafeWrite z i (r - i)
            calc i r (i + 1)
        -- starting with a known point
        | otherwise = do
            let !i' = i - l0
            !d0 <- min (n - i) <$> UM.unsafeRead z i'
            let !r = lcpSearch (i' + d0) (i + d0)
            UM.unsafeWrite z i (r - i)
            if r0 < r
              then calc i r (i + 1)
              else calc l0 r0 (i + 1)
      lcpSearch i' i
        | i >= n = i
        | c' == c = lcpSearch (i' + 1) (i + 1)
        | otherwise = i
        where
          c' = BSU.unsafeIndex bs i'
          c = BSU.unsafeIndex bs i

  calc 0 0 1
  return z
