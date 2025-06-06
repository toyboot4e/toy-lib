{-# LANGUAGE TypeFamilies #-}

module Data.Vector.Extra where

import Algorithm.Bisect
import Control.Monad.Primitive (PrimMonad)
import Data.Maybe
import Data.Ord (comparing)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VAI
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Stack (HasCallStack)

-- | \(O(N \log N)\) One dimensional index compression: xs -> (nubSortXs, xs')
{-# INLINE compressU #-}
compressU :: (HasCallStack) => U.Vector Int -> (U.Vector Int, U.Vector Int)
compressU xs = (dict, U.map (bindex dict) xs)
  where
    -- TODO: use radix sort
    -- NOTE: `U.modify VAI.sort` is super slow on GHC 9.4.5
    !dict = U.uniq $ U.modify (VAI.sortBy (comparing id)) xs

-- | \(O(\log N)\) Binary search-based indexing.
{-# INLINE bindex #-}
bindex :: (HasCallStack, G.Vector v a, Ord a) => v a -> a -> Int
bindex !dict !xref = fromJust $ bsearchL dict (<= xref)

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

-- | Wraps a boxed type as an `Unbox` instance, dispatching the boxed vector as the backing array
-- instance. <https://github.com/haskell/vector/issues/503>
newtype Boxed a = Boxed a

newtype instance U.MVector s (Boxed a) = MV_Boxed (V.MVector s a)

newtype instance U.Vector (Boxed a) = V_Boxed (V.Vector a)

instance GM.MVector UM.MVector (Boxed a) where
  {-# INLINE basicLength #-}
  basicLength = GM.basicLength
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice = GM.basicUnsafeSlice
  {-# INLINE basicOverlaps #-}
  basicOverlaps = GM.basicOverlaps
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew = GM.basicUnsafeNew
  {-# INLINE basicInitialize #-}
  basicInitialize = GM.basicInitialize
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead = GM.basicUnsafeRead
  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite = GM.basicUnsafeWrite
  {-# INLINE basicClear #-}
  basicClear = GM.basicClear
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy = GM.basicUnsafeCopy
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove = GM.basicUnsafeMove
  {-# INLINE basicUnsafeGrow #-}
  basicUnsafeGrow = GM.basicUnsafeGrow

instance G.Vector U.Vector (Boxed a) where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze = G.basicUnsafeFreeze
  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw = G.basicUnsafeThaw
  {-# INLINE basicLength #-}
  basicLength = G.basicLength
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice = G.basicUnsafeSlice
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM = G.basicUnsafeIndexM
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy = G.basicUnsafeCopy
  {-# INLINE elemseq #-}
  elemseq = G.elemseq

instance U.Unbox (Boxed a)
