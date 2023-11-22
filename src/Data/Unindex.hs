-- | Handy, more restricted `Data.Ix` with reverse conversion, fast monadic iteration and unboxing.
module Data.Unindex where

import Data.Ix
import qualified Data.Vector.Unboxed as U

class (Ix i, U.Unbox i) => Unindex i where
  -- TODO: Fusing
  -- https://wiki.haskell.org/GHC/Using_rules
  unindex :: (i, i) -> Int -> i

instance Unindex Int where
  unindex _ !v = v

instance Unindex (Int, Int) where
  unindex ((!y0, !x0), (!_, !x1)) !yx =
    let !w = x1 - x0 + 1
        (!dy, !dx) = yx `quotRem` w
     in (y0 + dy, x0 + dx)

instance Unindex (Int, Int, Int) where
  unindex ((!z0, !y0, !x0), (!_, !y1, !x1)) !zyx =
    let !h = y1 - y0 + 1
        !w = x1 - x0 + 1
        (!dz, !yx) = zyx `quotRem` (h * w)
        (!dy, !dx) = yx `quotRem` w
     in (z0 + dz, y0 + dy, x0 + dx)
