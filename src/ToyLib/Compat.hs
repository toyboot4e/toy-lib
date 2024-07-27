-- | Compatibilities.
module ToyLib.Compat where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.Trans.State.Strict (StateT (..))
import qualified Data.Vector.Generic.Mutable as GM
import Data.Bits ((.>>.))

-- | Original: https://hackage.haskell.org/package/transformers-0.6.1.1/docs/src/Control.Monad.Trans.State.Strict.html#modifyM
modifyM :: (Monad m) => (s -> m s) -> StateT s m ()
modifyM f = StateT $ \s -> do
  s' <- f s
  return ((), s')
{-# INLINE modifyM #-}

-- * next permutation

-- | Compute the (lexicographically) next permutation of the given vector in-place.
-- Returns False when the input is the last permutation; in this case the vector
-- will not get updated, as opposed to the behavior of the C++ function
-- @std::next_permutation@.
nextPermutation :: (PrimMonad m, Ord e, GM.MVector v e) => v (PrimState m) e -> m Bool
{-# INLINE nextPermutation #-}
nextPermutation = nextPermutationByLt (<)

-- | Compute the (lexicographically) next permutation of the given vector in-place,
-- using the provided comparison function.
-- Returns False when the input is the last permutation; in this case the vector
-- will not get updated, as opposed to the behavior of the C++ function
-- @std::next_permutation@.
nextPermutationBy :: (PrimMonad m, GM.MVector v e) => (e -> e -> Ordering) -> v (PrimState m) e -> m Bool
{-# INLINE nextPermutationBy #-}
nextPermutationBy cmp = nextPermutationByLt (\x y -> cmp x y == LT)

-- | Compute the (lexicographically) previous permutation of the given vector in-place.
-- Returns False when the input is the last permutation; in this case the vector
-- will not get updated, as opposed to the behavior of the C++ function
-- @std::next_permutation@.
prevPermutation :: (PrimMonad m, Ord e, GM.MVector v e) => v (PrimState m) e -> m Bool
{-# INLINE prevPermutation #-}
prevPermutation = nextPermutationByLt (>)

-- | Compute the (lexicographically) previous permutation of the given vector in-place,
-- using the provided comparison function.
-- Returns False when the input is the last permutation; in this case the vector
-- will not get updated, as opposed to the behavior of the C++ function
-- @std::next_permutation@.
prevPermutationBy :: (PrimMonad m, GM.MVector v e) => (e -> e -> Ordering) -> v (PrimState m) e -> m Bool
{-# INLINE prevPermutationBy #-}
prevPermutationBy cmp = nextPermutationByLt (\x y -> cmp x y == GT)

-- Here, the first argument should be a less-than comparison function.
-- Returns False when the input is the last permutation; in this case the vector
-- will not get updated, as opposed to the behavior of the C++ function
-- @std::next_permutation@.
nextPermutationByLt :: (PrimMonad m, GM.MVector v e) => (e -> e -> Bool) -> v (PrimState m) e -> m Bool
{-# INLINE nextPermutationByLt #-}
nextPermutationByLt lt v
  | dim < 2 = return False
  | otherwise = do
      !vlast <- GM.unsafeRead v (dim - 1)
      decrLoop (dim - 2) vlast
  where
    dim = GM.length v
    -- find the largest index k such that a[k] < a[k + 1], and then pass to the rest.
    decrLoop !i !vi1 | i >= 0 = do
      !vi <- GM.unsafeRead v i
      if vi `lt` vi1 then swapLoop i vi (i + 1) vi1 dim else decrLoop (i - 1) vi
    decrLoop _ !_ = return False
    -- find the largest index l greater than k such that a[k] < a[l], and do the rest.
    swapLoop !k !vk = go
      where
        -- binary search.
        go !l !vl !r | r - l <= 1 = do
          -- Done; do the rest of the algorithm.
          GM.unsafeWrite v k vl
          GM.unsafeWrite v l vk
          GM.reverse $ GM.unsafeSlice (k + 1) (dim - k - 1) v
          return True
        go !l !vl !r = do
          !vmid <- GM.unsafeRead v mid
          if vk `lt` vmid
            then go mid vmid r
            else go l vl mid
          where
            !mid = l + (r - l) .>>. 1
