{-# LANGUAGE TypeFamilies #-}

module Data.Vector.Extra where

import AtCoder.Extra.Bisect
import Control.Monad.Primitive (PrimMonad)
import Data.Maybe
import Data.Ord (comparing)
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import GHC.Stack (HasCallStack)

-- | \(O(N \log N)\) One dimensional index compression: xs -> (nubSortXs, xs')
{-# INLINE compressU #-}
compressU :: (HasCallStack) => U.Vector Int -> (U.Vector Int, U.Vector Int)
compressU xs = (dict, U.map (lowerBound dict) xs)
  where
    -- TODO: use radix sort
    -- NOTE: `U.modify VAI.sort` is super slow on GHC 9.4.5
    !dict = U.uniq $ U.modify (VAI.sortBy (comparing id)) xs

-- | \(O(N)\)
-- = Test
-- >>> chunksOfG 3 $ U.fromList ([1, 2, 3, 4, 5, 6, 7] :: [Int])
-- [[1,2,3],[4,5,6],[7]]
chunksOfG :: (G.Vector v a) => Int -> v a -> V.Vector (v a)
chunksOfG len xs0 = V.unfoldrExactN n step xs0
  where
    n = (G.length xs0 + len - 1) `div` len
    step xs = (G.take len xs, G.drop len xs)

-- | \(O(N)\)
-- = Test
-- >>> windowsOfG 3 $ U.fromList ([1, 2, 3, 4, 5, 6, 7] :: [Int])
-- [[1,2,3],[2,3,4],[3,4,5],[4,5,6],[5,6,7]]
windowsOfG :: (G.Vector v a) => Int -> v a -> V.Vector (v a)
windowsOfG len xs0 = V.generate (G.length xs0 - (len - 1)) $ \i ->
  G.take len $ G.drop i xs0

-- | \(O(N f)\) @U.constructN@ with monadic actions.
{-# INLINE constructMN #-}
constructMN :: forall a m. (PrimMonad m, U.Unbox a) => Int -> (U.Vector a -> m a) -> m (U.Vector a)
constructMN !n f = do
  v <- GM.new n
  v' <- G.unsafeFreeze v
  fill v' 0
  where
    fill :: U.Vector a -> Int -> m (U.Vector a)
    fill !v i
      | i < n = do
          x <- f (G.unsafeTake i v)
          G.elemseq v x $ do
            v' <- G.unsafeThaw v
            GM.unsafeWrite v' i x
            v'' <- G.unsafeFreeze v'
            fill v'' (i + 1)
    fill v _ = pure v

-- | \(O(N f)\) @U.constructrN@ with monadic actions.
{-# INLINE constructrMN #-}
constructrMN :: forall a m. (PrimMonad m, U.Unbox a) => Int -> (U.Vector a -> m a) -> m (U.Vector a)
constructrMN !n f = do
  v <- n `seq` GM.new n
  v' <- G.unsafeFreeze v
  fill v' 0
  where
    fill :: U.Vector a -> Int -> m (U.Vector a)
    fill !v i | i < n = do
      x <- f (G.unsafeSlice (n - i) i v)
      G.elemseq v x $ do
        v' <- G.unsafeThaw v
        GM.unsafeWrite v' (n - i - 1) x
        v'' <- G.unsafeFreeze v'
        fill v'' (i + 1)
    fill v _ = pure v
