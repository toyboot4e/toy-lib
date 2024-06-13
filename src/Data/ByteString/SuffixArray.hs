{-# LANGUAGE RecordWildCards #-}

-- | [Suffix Array](https://cp-algorithms.com/string/suffix-array.html) calculation.
--
-- = Definition
--
-- Suffix array is defined as @sa[i] = indexOf(sa[(n - 1 - i):])@ where @indexOf@ returns the index
-- after sort for all the suffixes.
module Data.ByteString.SuffixArray where

import qualified Data.ByteString.Char8 as BS
import Data.Ord
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VAI
import qualified Data.Vector.Unboxed as U

saOfNaive :: BS.ByteString -> U.Vector Int
saOfNaive bs =
  U.convert
    . V.map fst
    . V.modify (VAI.sortBy (comparing snd))
    $ V.generate n (\i -> (i, BS.drop (n - 1 - i) bs))
  where
    n = BS.length bs
