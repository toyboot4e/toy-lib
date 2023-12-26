{-# LANGUAGE LambdaCase #-}

module Data.Vector.Extra where

import Control.Monad
import Control.Monad.Extra (whenM)
import Control.Monad.Fix
import Control.Monad.ST
import Data.Buffer
import Data.Maybe
import Data.Ord (Down (..))
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

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

