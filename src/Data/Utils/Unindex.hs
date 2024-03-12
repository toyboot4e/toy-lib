-- | Handy, more restricted `Data.Ix` with reverse conversion, fast monadic iteration and unboxing.
module Data.Utils.Unindex where

import Data.Ix
import qualified Data.Vector.Unboxed as U

class (Ix i, U.Unbox i) => Unindex i where
  -- TODO: Fusing
  -- https://wiki.haskell.org/GHC/Using_rules
  unindex :: (i, i) -> Int -> i

instance Unindex Int where
  {-# INLINE unindex #-}
  unindex _ !v = v

instance Unindex (Int, Int) where
  {-# INLINE unindex #-}
  unindex ((!y0, !x0), (!_, !x1)) !yx =
    let !w = x1 - x0 + 1
        (!dy, !dx) = yx `quotRem` w
     in (y0 + dy, x0 + dx)

instance Unindex (Int, Int, Int) where
  {-# INLINE unindex #-}
  unindex ((!z0, !y0, !x0), (!_, !y1, !x1)) !zyx =
    let !h = y1 - y0 + 1
        !w = x1 - x0 + 1
        (!dz, !yx) = zyx `quotRem` (h * w)
        (!dy, !dx) = yx `quotRem` w
     in (z0 + dz, y0 + dy, x0 + dx)

instance Unindex (Int, Int, Int, Int) where
  {-# INLINE unindex #-}
  unindex ((!b3, !b2, !b1, !b0), (!_, !x2, !x1, !x0)) !pos3 =
    let !w2 = x2 - b2 + 1
        !w1 = x1 - b1 + 1
        !w0 = x0 - b0 + 1
        (!y3, !pos2) = pos3 `quotRem` (w2 * w1 * w0)
        (!y2, !pos1) = pos2 `quotRem` (w1 * w0)
        (!y1, !y0) = pos1 `quotRem` w0
     in (b3 + y3, b2 + y2, b1 + y1, b0 + y0)

instance Unindex ((Int, Int), (Int, Int)) where
  {-# INLINE unindex #-}
  unindex (((!b3, !b2), (!b1, !b0)), ((!_, !x2), (!x1, !x0))) !pos3 =
    let !w2 = x2 - b2 + 1
        !w1 = x1 - b1 + 1
        !w0 = x0 - b0 + 1
        (!y3, !pos2) = pos3 `quotRem` (w2 * w1 * w0)
        (!y2, !pos1) = pos2 `quotRem` (w1 * w0)
        (!y1, !y0) = pos1 `quotRem` w0
     in ((b3 + y3, b2 + y2), (b1 + y1, b0 + y0))
