-- | Index compression.
module Data.Vector.Compress where

import Algorithm.Bisect
import Data.Maybe
import Data.Ord (comparing)
import qualified Data.Vector.Algorithms.Intro as VAI
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import GHC.Stack (HasCallStack)

-- | One dimensional index compression: xs -> (nubSortXs, xs')
{-# INLINE compressU #-}
compressU :: (HasCallStack) => U.Vector Int -> (U.Vector Int, U.Vector Int)
compressU xs = (dict, U.map (bindex dict) xs)
  where
    -- TODO: use radix sort
    -- NOTE: `U.modify VAI.sort` is super slow on GHC 9.4.5
    !dict = U.uniq $ U.modify (VAI.sortBy (comparing id)) xs

-- | Binary search-based indexing.
{-# INLINE bindex #-}
bindex :: (HasCallStack, G.Vector v a, Ord a) => v a -> a -> Int
bindex !dict !xref = fromJust $ bsearchL dict (<= xref)
