{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Data.SegmentTree.Util where

import Data.Bits
import Data.Core.SemigroupAction
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- | \(O(1)\)
{-# INLINE _childL #-}
_childL :: Int -> Int
_childL !vertex = vertex .<<. 1

-- | \(O(1)\)
{-# INLINE _childR #-}
_childR :: Int -> Int
_childR !vertex = (vertex .<<. 1) .|. 1

-- | \(O(1)\)
{-# INLINE _isLChild #-}
_isLChild :: Int -> Bool
_isLChild = not . (`testBit` 0)

-- | \(O(1)\)
{-# INLINE _isRChild #-}
_isRChild :: Int -> Bool
_isRChild = (`testBit` 0)

-- | \(O(1)\) Pruning trick for excluding vertices under the glitch segments.
{-# INLINE _pruneTrick #-}
_pruneTrick :: Int -> Int -> Int -> Bool
_pruneTrick vLeaf iParent lrAdjuster = (vLeaf + lrAdjuster) .>>. iParent .<<. iParent /= (vLeaf + lrAdjuster)

-- * Segment tree beats

class FailableSemigroupActionTarget a where
  isFailureFSAT :: a -> Bool
