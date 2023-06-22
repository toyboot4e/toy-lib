{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Data.Vector.InvNum where

import Control.Monad.ST (runST)
import Data.Maybe
import Data.SegmentTree.Strict
import Data.Vector.Compress (compressVU)
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import ToyLib.Prelude (foldForMVG)

-- {{{ Inveresion number (segment tree)

-- | Calculates the inversion number.
invNumVG :: Int -> (VG.Vector v Int) => v Int -> Int
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
    return $ acc + s

-- | Calculates the inversion number after applying index compression.
-- It can significantly improve the performance, like in ABC 261 F.
compressInvNumVG :: VU.Vector Int -> Int
compressInvNumVG xs = invNumVG (pred (VU.length xs')) xs'
  where
    !xs' = snd $ compressVU xs

-- }}}
