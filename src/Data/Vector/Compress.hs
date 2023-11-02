module Data.Vector.Compress where

import Algorithm.BinarySearch
import Data.List.Extra (nubSort)
import Data.Maybe
import qualified Data.Vector.Unboxed as U
import GHC.Stack (HasCallStack)

-- | One dimensional index compression: xs -> (indexer, xs')
compressVU :: HasCallStack => U.Vector Int -> (U.Vector Int, U.Vector Int)
compressVU xs = (indexer, U.map (fromJust . fst . f) xs)
  where
    !indexer = U.fromList $ nubSort $ U.toList xs
    f !x = bsearch (0, pred $ U.length indexer) $ \i -> indexer U.! i <= x
