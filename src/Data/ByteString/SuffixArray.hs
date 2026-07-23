-- | [Suffix Array](https://cp-algorithms.com/string/suffix-array.html) calculation.
--
-- = Definition
--
-- \(\mathcal{sa}[i] = \mathcal{originalOrderOf}(\mathcal{sa}[(n-1-i):])\) where
-- \(\mathcal{originalOrderOf(i)}\) returns the order of i-th suffix.
module Data.ByteString.SuffixArray where

import AtCoder.String qualified as AS
import Data.ByteString.Char8 qualified as BS
import Data.Vector.Unboxed qualified as VU

countUniqueSubstringsBS :: BS.ByteString -> Int
countUniqueSubstringsBS bs = (n * (n + 1)) `div` 2 - VU.sum lcp
  where
    n = BS.length bs
    sa = AS.suffixArrayBS bs
    lcp = AS.lcpArrayBS bs sa
