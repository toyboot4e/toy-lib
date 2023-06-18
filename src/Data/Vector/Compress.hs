{-# LANGUAGE BangPatterns #-}

module Data.Vector.Compress where

import Algorithm.BinarySearch
import Data.List.Extra (nubSort)
import Data.Maybe
import qualified Data.Vector.Unboxed as VU
import ToyLib.Prelude (vLength)

-- | One dimensional index compression: xs -> (indices, xs')
compressVU :: VU.Vector Int -> (VU.Vector Int, VU.Vector Int)
compressVU xs = (indices, VU.map (fromJust . fst . f) xs)
  where
    !indices = VU.fromList $ nubSort $ VU.toList xs
    f !x = bsearch (0, pred $ vLength indices) $ \i -> indices VU.! i <= x
