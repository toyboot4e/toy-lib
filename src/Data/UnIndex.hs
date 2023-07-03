{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Handy, more restricted `Data.Ix` with reverse conversion, fast monadic iteration and unboxing.
module Data.UnIndex where

import Data.Ix
import qualified Data.Vector.Fusion.Stream.Monadic as MS
import qualified Data.Vector.Unboxed as VU
import ToyLib.Prelude (rangeMS)

class RangeMS a where
  rangeMS2 :: Monad m => (a, a) -> MS.Stream m a

instance RangeMS Int where
  rangeMS2 (!l, !r) = rangeMS l r

instance RangeMS (Int, Int) where
  rangeMS2 ((!y0, !x0), (!y1, !x1)) = MS.Stream step (y0, x0)
    where
      {-# INLINE [0] step #-}
      step (!y, !x)
        | x <= x1 = return $ MS.Yield (y, x) (y, x + 1)
        | y <= y1 = return $ MS.Yield (y, x) (y + 1, x0)
        | otherwise = return MS.Done

class (Ix i, VU.Unbox i) => UnIndex i where
  -- TODO: Fusing
  -- https://wiki.haskell.org/GHC/Using_rules
  unindex :: (i, i) -> Int -> i

instance UnIndex Int where
  unindex _ !v = v

instance UnIndex (Int, Int) where
  unindex ((!y0, !x0), (!y1, !x1)) !yx =
    let !w = x1 - x0
     in yx `quotRem` w

instance UnIndex (Int, Int, Int) where
  unindex ((!z0, !y0, !x0), (!z1, !y1, !x1)) !zyx =
    let !d = z1 - z0
        !h = y1 - y0
        !w = x1 - x0
        (!z, !yx) = zyx `quotRem` (h * w)
        (!y, !x) = yx `quotRem` w
     in (z, y, x)

