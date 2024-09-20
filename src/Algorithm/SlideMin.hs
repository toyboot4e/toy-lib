-- | Sliding minimum window.
module Algorithm.SlideMin where

import Control.Monad.Extra (whenM)
import Control.Monad.Fix
import Control.Monad.ST
import Data.Buffer
import Data.Ord (Down(..))
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

-- | \(O(N)\) <https://qiita.com/kuuso1/items/318d42cd089a49eeb332>
--
-- TODO: Explain what it's like.
--
-- Returns indices of elements of the minimum values in the windows with the specified length.
{-# INLINE slideMaxIndicesOn #-}
slideMaxIndicesOn :: (G.Vector v a, Ord b) => (a -> b) -> Int -> v a -> U.Vector Int
slideMaxIndicesOn wrap len xs = runST $ do
  -- queue of minimum numbers
  !buf <- newBuffer (G.length xs)

  fmap (U.drop (len - 1)) $ G.generateM (G.length xs) $ \i -> do
    -- remove indices that are no longer in the span
    fix $ \loop -> do
      whenM (maybe False (<= i - len) <$> readMaybeFront buf 0) $ do
        popFront_ buf
        loop

    -- remove indices that are less attractive to the new coming value
    fix $ \loop -> do
      whenM (maybe False ((< wrap (xs G.! i)) . wrap . (xs G.!)) <$> readMaybeBack buf 0) $ do
        popBack_ buf
        loop

    pushBack buf i
    readFront buf 0

-- | \(O(N)\)
--
-- = Example
--
-- @
-- indices: 0 1 2 3 4 5
-- values:  5 4 3 2 1 0   min value indices:
--          [---]         2
--            [---]       3
--              [---]     4
--                [---]   5
-- @
--
-- >>> slideMinIndices 3 (U.fromList [0 .. 5])
-- [0,1,2,3]
-- >>> slideMinIndices 3 (U.fromList [5, 4 .. 0])
-- [2,3,4,5]
{-# INLINE slideMinIndices #-}
slideMinIndices :: Int -> U.Vector Int -> U.Vector Int
slideMinIndices = slideMaxIndicesOn Down

-- | \(O(N)\)
--
-- >>> slideMaxIndices 3 (U.fromList [0 .. 5])
-- [2,3,4,5]
-- >>> slideMaxIndices 3 (U.fromList [5, 4 .. 0])
-- [0,1,2,3]
{-# INLINE slideMaxIndices #-}
slideMaxIndices :: Int -> U.Vector Int -> U.Vector Int
slideMaxIndices = slideMaxIndicesOn id
