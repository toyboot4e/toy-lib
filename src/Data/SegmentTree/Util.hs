{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Data.SegmentTree.Util where

import Data.Bits
import Data.Coerce
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

-- * Semigroup action with length

-- | Right semigroup aciton with length given by the segment tree. Any `Semigroup` can be coerced
-- into `SemigroupActionWithLength` using the `WithLength` newtype.
class SemigroupActionWithLength s a where
  -- | Right semigroup aciton with length given by the segment tree.
  sactWithLength :: s -> a -> Int -> a

-- | (Internal) Wraps a `SemigroupAction` instance into a `SemigroupActionWithLength`.
newtype WithLength a = WithLength a
  deriving newtype (Eq, Ord)

instance (Semigroup a) => Semigroup (WithLength a) where
  {-# INLINE (<>) #-}
  (<>) = coerce ((<>) @a)

instance (Monoid a) => Monoid (WithLength a) where
  {-# INLINE mempty #-}
  mempty = coerce (mempty @a)

instance (SemigroupAction s a) => SemigroupActionWithLength (WithLength s) a where
  {-# INLINE sactWithLength #-}
  sactWithLength s a _ = coerce (sact @s @a) s a

{- ORMOLU_DISABLE -}
newtype instance U.MVector s (WithLength a) = MV_WithLength (U.MVector s a)
newtype instance U.Vector (WithLength a) = V_WithLength (U.Vector a)
deriving instance (U.Unbox a) => GM.MVector UM.MVector (WithLength a)
deriving instance (U.Unbox a) => G.Vector U.Vector (WithLength a)
instance (U.Unbox a) => U.Unbox (WithLength a)
{- ORMOLU_ENABLE -}

-- * Segment tree beats

class FailableSemigroupActionTarget a where
  isFailureFSAT :: a -> Bool
