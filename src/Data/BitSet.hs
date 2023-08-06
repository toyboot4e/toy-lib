-- | Bit set tricks
module Data.BitSet where

import Control.Monad
import Data.Bits
import Data.List (unfoldr)
import Data.Tuple.Extra (dupe)
import qualified Data.Vector.Unboxed as VU

-- | Originally by @yamate11
powersetM_ :: (Bits a, Num a, Monad m) => a -> (a -> m ()) -> m ()
powersetM_ !is0 !act = act2 is0
  where
    act2 !is = do
      act is
      unless (is == 0) (act2 (is0 .&. (is - 1)))

-- | Originally by @yamate11
powerset :: (Bits a, Num a) => a -> [a]
powerset !a = a : unfoldr f a
  where
    f 0 = Nothing
    f !x = Just . dupe $! a .&. (x - 1)

-- | TODO: Working as expected?
powersetVU :: (Bits a, Num a, VU.Unbox a) => a -> VU.Vector a
powersetVU !a = VU.unfoldr f a
  where
    f (-1) = Nothing
    f 0 = Just (0, -1)
    f !x =
      let !x' = a .&. (x - 1)
       in Just (x, x')
