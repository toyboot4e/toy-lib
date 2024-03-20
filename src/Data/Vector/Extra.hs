{-# LANGUAGE LambdaCase #-}

module Data.Vector.Extra where

import Algorithm.Bisect
import Control.Monad
import Control.Monad.Extra (whenM)
import Control.Monad.Fix
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Buffer
import Data.Maybe
import Data.Monoid
import Data.Ord (comparing, Down(..))
import Data.SegmentTree.Strict
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VAI
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import GHC.Stack (HasCallStack)
import Math.PowMod (factModsN)

-- | One dimensional index compression: xs -> (nubSortXs, xs')
{-# INLINE compressU #-}
compressU :: (HasCallStack) => U.Vector Int -> (U.Vector Int, U.Vector Int)
compressU xs = (dict, U.map (bindex dict) xs)
  where
    -- TODO: use radix sort
    -- NOTE: `U.modify VAI.sort` is super slow on GHC 9.4.5
    !dict = U.uniq $ U.modify (VAI.sortBy (comparing id)) xs

-- | Binary search-based indexing.
{-# INLINE bindex #-}
bindex :: (HasCallStack, G.Vector v a, Ord a) => v a -> a -> Int
bindex !dict !xref = fromJust $ bsearchL dict (<= xref)

-- | = Test
-- >>> chunksOfG 3 $ U.fromList ([1, 2, 3, 4, 5, 6, 7] :: [Int])
-- [[1,2,3],[4,5,6],[7]]
chunksOfG :: (G.Vector v a) => Int -> v a -> V.Vector (v a)
chunksOfG k xs0 = V.unfoldrExactN n step xs0
  where
    n = (G.length xs0 + k - 1) `div` k
    step xs = (G.take k xs, G.drop k xs)

-- | <https://qiita.com/kuuso1/items/318d42cd089a49eeb332>
--
-- Returns indices of elements of the minimum values in corresponding spans.
-- Drop the first @len - 1@ elements if you don't need spans shorter than the @len@.
{-# INLINE slideMinIndicesOn #-}
slideMinIndicesOn :: (G.Vector v a, Ord b) => (a -> b) -> Int -> v a -> U.Vector Int
slideMinIndicesOn wrap len xs = runST $ do
  -- queue of minimum numbers
  !buf <- newBufferAsQueue (G.length xs)

  G.generateM (G.length xs) $ \i -> do
    -- remove indices that are no longer in the span
    fix $ \loop -> do
      whenM (maybe False (<= i - len) <$> viewFront buf) $ do
        void $ popFront buf
        loop

    -- remove indices that are less attractive to the new combing value
    fix $ \loop -> do
      whenM (maybe False ((< wrap (xs G.! i)) . wrap . (xs G.!)) <$> viewBack buf) $ do
        void $ popBack buf
        loop

    pushBack buf i
    fromJust <$> viewFront buf

-- | >>> slideMinIndices 3 (U.fromList [0 .. 5])
-- [0,1,2,3,4,5]
-- >>> slideMinIndices 3 (U.fromList [5, 4 .. 0])
-- [0,0,0,1,2,3]
{-# INLINE slideMinIndices #-}
slideMinIndices :: Int -> U.Vector Int -> U.Vector Int
slideMinIndices = slideMinIndicesOn id

-- | >>> slideMaxIndices 3 (U.fromList [0 .. 5])
-- [0,0,0,1,2,3]
-- >>> slideMaxIndices 3 (U.fromList [5, 4 .. 0])
-- [0,1,2,3,4,5]
{-# INLINE slideMaxIndices #-}
slideMaxIndices :: Int -> U.Vector Int -> U.Vector Int
slideMaxIndices = slideMinIndicesOn Down

{-# INLINE constructNM #-}
constructNM :: forall a m. (PrimMonad m, U.Unbox a) => Int -> (U.Vector a -> m a) -> m (U.Vector a)
constructNM !n f = do
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

{-# INLINE constructrNM #-}
constructrNM :: forall a m. (PrimMonad m, U.Unbox a) => Int -> (U.Vector a -> m a) -> m (U.Vector a)
constructrNM !n f = do
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

prevPermutation :: (Ord e, G.Vector v e, G.Vector v (Down e)) => v e -> v e
prevPermutation =
  G.map (\case Down !x -> x)
    . G.modify (void . GM.nextPermutation)
    . G.map Down

-- | Calculates the inversion number. Be sure to compress the input vector!
--
-- = Typical problems
--
-- TODO
--
-- - ABC 261 - F
invNum :: (HasCallStack) => Int -> (G.Vector v Int) => v Int -> Int
invNum xMax xs = runST $ do
  !stree <- newSTree @(Sum Int) (xMax + 1)

  fmap getSum . (\f -> G.foldM' f mempty xs) $ \acc x -> do
    -- count pre-inserted numbers bigger than this:
    !s <- fromMaybe mempty <$> foldMaySTree stree (x + 1) xMax
    modifySTree stree (+ 1) x
    return $! acc + s

-- | Calculates the inversion number, but after applying index compression.
-- It can significantly improve the performance, like in ABC 261 F.
compressInvNumG :: (HasCallStack) => U.Vector Int -> Int
compressInvNumG xs = invNum (U.length xs' - 1) xs'
  where
    !xs' = snd $ compressU xs

-- | Returns 1-based lexicographic order of the given array.
--
-- WARNING: Use 0-based indices for the input.
--
-- = Typical problems
--
-- TODO
lexOrderMod :: (HasCallStack, G.Vector v Int) => v Int -> Int -> Int
lexOrderMod xs modulo = runST $ do
  !stree <- newSTree @(Sum Int) (G.length xs + 1)

  -- Pre-calculated factorial numbers:
  let !facts = factModsN modulo (G.length xs)

  -- The calculation is very similar to that of inversion number. For example,
  -- @
  --     2 0 4 3 1
  --     | | | | |
  --     | | | | +-- 0 * 0!
  --     | | | +-- 1 * 1!
  --     | | +-- 2 * 2!
  --     | +-- 0 * 3 !
  --     +-- 2 * 4!
  -- @
  -- So each expression is given as `(the number of unused numbers smaller than this) * factMod`.
  !counts <- G.iforM xs $ \i x -> do
    Sum !nUsed <- foldSTree stree 0 x
    let !nUnused = x - nUsed
    let !factMod = facts G.! (G.length xs - (i + 1))
    let !inc = nUnused * factMod `rem` modulo

    -- mark it as used
    writeSTree stree x (Sum 1)

    return inc

  return $ (+ 1) $ G.foldl1' (\ !acc x -> (acc + x) `rem` modulo) counts
