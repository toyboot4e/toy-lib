{-# LANGUAGE LambdaCase #-}

module Data.Vector.Extra where

import qualified Data.Vector as V
import qualified Data.Vector.Generic as G

-- | = Test
-- >>> chunksOfG 3 $ U.fromList ([1, 2, 3, 4, 5, 6, 7] :: [Int])
-- [[1,2,3],[4,5,6],[7]]
chunksOfG :: (G.Vector v a) => Int -> v a -> V.Vector (v a)
chunksOfG k xs0 = V.unfoldrExactN n step xs0
  where
    n = (G.length xs0 + k - 1) `div` k
    step xs = (G.take k xs, G.drop k xs)

