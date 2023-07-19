{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Data.Vector.DictOrder where

import Control.Monad (void)
import Control.Monad.ST (runST)
import Data.Maybe
import Data.SegmentTree.Strict
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import GHC.Exts
import Math.PowMod (factMods)

-- {{{ Dictionary orders

prevPermutationVec :: (Ord e, VG.Vector v e, VG.Vector v (Down e)) => v e -> v e
prevPermutationVec =
  VG.map (\case Down !x -> x)
    . VG.modify (void . VGM.nextPermutation)
    . VG.map Down

-- | Returns 1-based dictionary order for the given array.
-- WARNING: Use 0-based indices for the input.
dictOrderModuloVec :: (VG.Vector v Int) => v Int -> Int -> Int
dictOrderModuloVec xs modulo = runST $ do
  !stree <- newSTreeVU (+) (VG.length xs + 1) (0 :: Int)

  -- Pre-calculate factorial numbers:
  let !facts = factMods (VG.length xs) modulo

  -- The calculation is very similar to that of inversion number. For example,
  -- ```
  --     2 0 4 3 1
  --     | | | | |
  --     | | | | +-- 0 * 0!
  --     | | | +-- 1 * 1!
  --     | | +-- 2 * 2!
  --     | +-- 0 * 3 !
  --     +-- 2 * 4!
  -- ```
  -- So each expression is given as `(the number of unused numbers smaller than this) * factMod`.
  !counts <- flip VG.imapM xs $ \i x -> do
    !nUsed <- fromJust <$> querySTree stree (0, x)
    let !nUnused = x - nUsed
    let !factMod = facts VG.! (VG.length xs - (i + 1))
    let !inc = nUnused * factMod `rem` modulo

    -- mark it as used
    insertSTree stree x 1

    return inc

  return $! succ $! VG.foldl1' (\ !acc x -> (acc + x) `rem` modulo) counts

-- }}}
