-- | Index compression.
module Data.Vector.Compress where

import Algorithm.Bisect
import Data.Maybe
import qualified Data.Vector.Algorithms.Intro as VAI
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as G
import GHC.Stack (HasCallStack)

-- | One dimensional index compression: xs -> (nubSortXs, xs')
{-# INLINE compressU #-}
compressU :: (HasCallStack) => U.Vector Int -> (U.Vector Int, U.Vector Int)
compressU xs = (nubSortXs, U.map (bindex nubSortXs) xs)
  where
    -- TODO: use radix sort
    !nubSortXs = U.uniq $ U.modify VAI.sort xs

-- | Index with binary search (and index compression)
{-# INLINE bindex #-}
bindex :: (HasCallStack, G.Vector v a, Ord a) => v a -> a -> Int
bindex !vec !xref = fromJust $ bsearchL vec (<= xref)

