{-# LANGUAGE LambdaCase #-}

module Data.Vector.InvNum where

-- | Inversion number calculation with `SegmentTree`

import Control.Monad.ST (runST)
import Data.Maybe
import Data.SegmentTree.Strict
import Data.Vector.Compress (compressVU)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import ToyLib.Prelude (foldForMVG)
import GHC.Stack (HasCallStack)

-- | Calculates the inversion number. Be sure to compress the input vector!
invNumVG :: HasCallStack => Int -> (G.Vector v Int) => v Int -> Int
invNumVG xMax xs = runST $ do
  !stree <- newSTreeVU (+) (xMax + 1) (0 :: Int)

  foldForMVG (0 :: Int) xs $ \acc x -> do
    -- count pre-inserted numbers bigger than this:
    -- let !_ = dbg (x, (succ x, xMax))
    !s <-
      if x == xMax
        then return 0
        else fromJust <$> querySTree stree (succ x, xMax)

    -- let !_ = traceShow (x, s, (succ x, pred n)) ()
    modifySTree stree succ x
    return $! acc + s

-- | Calculates the inversion number after applying index compression.
-- It can significantly improve the performance, like in ABC 261 F.
compressInvNumVG :: HasCallStack => U.Vector Int -> Int
compressInvNumVG xs = invNumVG (pred (U.length xs')) xs'
  where
    !xs' = snd $ compressVU xs
