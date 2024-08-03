-- | Lazily segment tree with interval chmax/chmin.
--
-- Segment tree beats can also handle gcd queries, but it's not considered in this module.
module Data.SegmentTree.Beats where

import Data.SegmentTree.Lazy

type SegmentTreeBeats a op s = LazySegmentTree a op s
