-- | [Suffix Array](https://cp-algorithms.com/string/suffix-array.html) calculation.
--
-- = Definition
--
-- \(\mathcal{sa}[i] = \mathcal{originalOrderOf}(\mathcal{sa}[(n-1-i):])\) where
-- \(\mathcal{originalOrderOf(i)}\) returns the order of i-th suffix.
module Data.ByteString.SuffixArray where

import qualified AtCoder.String as AS
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Unboxed as VU

countUniqueSubstringsBS :: BS.ByteString -> Int
countUniqueSubstringsBS bs = (n * (n + 1)) `div` 2 - VU.sum lcp
  where
    n = BS.length bs
    sa = AS.suffixArrayBS bs
    lcp = AS.lcpArrayBS bs sa
