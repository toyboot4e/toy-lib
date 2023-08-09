module Data.Vector.Compress where

import Algorithm.BinarySearch
import Data.List.Extra (nubSort)
import Data.Maybe
import qualified Data.Vector.Unboxed as VU

-- | One dimensional index compression: xs -> (indexer, xs')
compressVU :: VU.Vector Int -> (VU.Vector Int, VU.Vector Int)
compressVU xs = (indexer, VU.map (fromJust . fst . f) xs)
  where
    !indexer = VU.fromList $ nubSort $ VU.toList xs
    f !x = bsearch (0, pred $ VU.length indexer) $ \i -> indexer VU.! i <= x
