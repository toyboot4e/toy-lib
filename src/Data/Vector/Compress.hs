-- | Index compression.
module Data.Vector.Compress where

import Algorithm.Bisect
import Data.Maybe
import qualified Data.Vector.Algorithms.Intro as VAI
import qualified Data.Vector.Unboxed as U
import GHC.Stack (HasCallStack)

-- | One dimensional index compression: xs -> (nubSortXs, is)
compressU :: (HasCallStack) => U.Vector Int -> (U.Vector Int, U.Vector Int)
compressU xs = (indexer, U.map (fromJust . fst . f) xs)
  where
    -- !indexer = U.fromList $ nubSort $ U.toList xs
    !indexer = U.uniq $ U.modify VAI.sort xs
    f !x = bisect 0 (pred (U.length indexer)) $ \i -> indexer U.! i <= x
