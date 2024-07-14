-- | Sliding minimum.
module Data.Vector.SlideMin where

import Control.Monad
import Control.Monad.Extra (whenM)
import Control.Monad.Fix
import Control.Monad.ST
import Data.Buffer
import Data.Maybe
import Data.Ord (Down(..))
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

-- | \(O(N)\) <https://qiita.com/kuuso1/items/318d42cd089a49eeb332>
--
-- Returns indices of elements of the minimum values in corresponding spans.
-- Drop the first @len - 1@ elements if you don't need spans shorter than the @len@.
{-# INLINE slideMinIndicesOn #-}
slideMinIndicesOn :: (G.Vector v a, Ord b) => (a -> b) -> Int -> v a -> U.Vector Int
slideMinIndicesOn wrap len xs = runST $ do
  -- queue of minimum numbers
  !buf <- newBuffer (G.length xs)

  G.generateM (G.length xs) $ \i -> do
    -- remove indices that are no longer in the span
    fix $ \loop -> do
      whenM (maybe False (<= i - len) <$> readMaybeFront buf 0) $ do
        void $ popFront buf
        loop

    -- remove indices that are less attractive to the new combing value
    fix $ \loop -> do
      whenM (maybe False ((< wrap (xs G.! i)) . wrap . (xs G.!)) <$> readMaybeBack buf 0) $ do
        void $ popBack buf
        loop

    pushBack buf i
    fromJust <$> readMaybeFront buf 0

-- | \(O(N)\)
--
-- >>> slideMinIndices 3 (U.fromList [0 .. 5])
-- [0,1,2,3,4,5]
-- >>> slideMinIndices 3 (U.fromList [5, 4 .. 0])
-- [0,0,0,1,2,3]
{-# INLINE slideMinIndices #-}
slideMinIndices :: Int -> U.Vector Int -> U.Vector Int
slideMinIndices = slideMinIndicesOn id

-- | \(O(N)\)
--
-- >>> slideMaxIndices 3 (U.fromList [0 .. 5])
-- [0,0,0,1,2,3]
-- >>> slideMaxIndices 3 (U.fromList [5, 4 .. 0])
-- [0,1,2,3,4,5]
{-# INLINE slideMaxIndices #-}
slideMaxIndices :: Int -> U.Vector Int -> U.Vector Int
slideMaxIndices = slideMinIndicesOn Down
