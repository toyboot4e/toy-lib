{-# LANGUAGE LambdaCase #-}

module Data.Vector.Extra where

import Algorithm.Bisect
import Control.Monad
import Control.Monad.Primitive
import Data.Maybe
import Data.Ord (comparing, Down(..))
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VAI
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
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
chunksOfG k xs0 = V.unfoldrExactN n step xs0
  where
    n = (G.length xs0 + k - 1) `div` k
    step xs = (G.take k xs, G.drop k xs)

-- | \(O(N f)\)
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
    fill v _ = return v

-- | \(O(N f)\)
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
    fill v _ = return v

-- | Faster @nextPermutaions@ (as of today).
nextPermutation :: (PrimMonad m, Ord e, GM.MVector v e) => v (PrimState m) e -> m Bool
nextPermutation v
  | dim < 2 = return False
  | otherwise = stToPrim $ do
      val <- GM.unsafeRead v 0
      (k, l) <- loop val (-1) 0 val 1
      if k < 0
        then return False
        else
          GM.unsafeSwap v k l
            >> GM.reverse (GM.unsafeSlice (k + 1) (dim - k - 1) v)
            >> return True
  where
    loop !kval !k !l !prev !i
      | i == dim = return (k, l)
      | otherwise = do
          cur <- GM.unsafeRead v i
          -- the line modified from
          let (!kval', !k') = if prev < cur then (prev, i - 1) else (kval, k)
              l' = if kval' < cur then i else l
          loop kval' k' l' cur (i + 1)
    dim = GM.length v

-- | TODO: coerce?
prevPermutation :: (Ord e, G.Vector v e, G.Vector v (Down e)) => v e -> v e
prevPermutation =
  G.map (\case Down !x -> x)
    . G.modify (void . nextPermutation)
    . G.map Down
