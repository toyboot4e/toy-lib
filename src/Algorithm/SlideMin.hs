-- | Sliding minimum window.
module Algorithm.SlideMin where

import Control.Monad.Extra (whenM)
import Control.Monad.Fix
import Control.Monad.ST
import Data.Buffer
import Data.Maybe
import Data.Ord (Down(..))
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

-- | \(O(N)\) (1) in <https://qiita.com/kuuso1/items/318d42cd089a49eeb332>
{-# INLINE slideCmpIndicesOn #-}
slideCmpIndicesOn :: (G.Vector v a, Ord b) => (a -> b) -> Int -> v a -> U.Vector Int
slideCmpIndicesOn wrap len xs = runST $ do
  -- dequeue of maximum number indices.
  !buf <- newBuffer (G.length xs)

  fmap (U.drop (len - 1)) $ G.generateM (G.length xs) $ \i -> do
    -- remove the front indices that are no longer in the span
    fix $ \loop -> do
      whenM (maybe False (<= i - len) <$> readMaybeFront buf 0) $ do
        popFront_ buf
        loop

    -- remove the last indices that are less attractive to the new coming value
    fix $ \loop -> do
      whenM (maybe False ((< wrap (xs G.! i)) . wrap . (xs G.!)) <$> readMaybeBack buf 0) $ do
        popBack_ buf
        loop

    pushBack buf i
    readFront buf 0

-- | \(O(N)\) Returns indices of minimum values in the windows with the specified length.
--
-- >>> slideMinIndices 3 (U.fromList [0 .. 5])
-- [0,1,2,3]
-- >>> slideMinIndices 3 (U.fromList [5, 4 .. 0])
-- [2,3,4,5]
{-# INLINE slideMinIndices #-}
slideMinIndices :: Int -> U.Vector Int -> U.Vector Int
slideMinIndices = slideCmpIndicesOn Down

-- | \(O(N)\) Returns indices of maximum values in the windows with the specified length.
--
-- = Example
--
-- @
-- indices: 0 1 2 3 4 5
-- values:  0 1 2 3 4 5   max value indices:
--          [---]         2
--            [---]       3
--              [---]     4
--                [---]   5
-- @
--
-- >>> slideMaxIndices 3 (U.fromList [0 .. 5])
-- [2,3,4,5]
-- >>> slideMaxIndices 3 (U.fromList [5, 4 .. 0])
-- [0,1,2,3]
{-# INLINE slideMaxIndices #-}
slideMaxIndices :: Int -> U.Vector Int -> U.Vector Int
slideMaxIndices = slideCmpIndicesOn id

-- | \(O(N)\) (2) <https://qiita.com/kuuso1/items/318d42cd089a49eeb332>
{-# INLINE lookBackIndicesOn #-}
lookBackIndicesOn :: (G.Vector v a, Ord b) => (a -> b) -> v a -> U.Vector Int
lookBackIndicesOn wrap xs = runST $ do
  -- dequeue of maximum number indices. actually a stack is enough though.
  !buf <- newBuffer (G.length xs)

  G.generateM (G.length xs) $ \i -> do
    -- remove the last indices that are less than or equal to  attractive to the new coming value:
    fix $ \loop -> do
      whenM (maybe False ((< wrap (xs G.! i)) . wrap . (xs G.!)) <$> readMaybeBack buf 0) $ do
        popBack_ buf
        loop

    res <- fromMaybe (-1) <$> readMaybeBack buf 0
    pushBack buf i
    pure res

-- | \(O(N)\) Solution to the histogram problem. Find the nearest lower building for each @i@..
{-# INLINE lookBackLowerIndices #-}
lookBackLowerIndices :: U.Vector Int -> U.Vector Int
lookBackLowerIndices = lookBackIndicesOn Down

-- | \(O(N)\) Solution to the histogram problem. Find the nearest higher building for each @i@..
--
-- = Example
--
-- @
-- index:  -1  0   1   2   3   4
-- height: --  1   5   2   4   3
--           <---- |
--                 | <---- |
--                 |       | <-|
--                 | <-|   |   |
--           <-|   |   |   |   |
-- look back: -1  -1   1   1   3
-- @
--
-- = Typical problems
-- - [ABC 372 D  - Buildings](https://atcoder.jp/contests/abc372/tasks/abc372_d)
{-# INLINE lookBackHigherIndices #-}
lookBackHigherIndices :: U.Vector Int -> U.Vector Int
lookBackHigherIndices = lookBackIndicesOn id
