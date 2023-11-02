{-# LANGUAGE LambdaCase #-}

module Data.Vector.DictOrder where

import Control.Monad (void)
import Control.Monad.ST (runST)
import Data.Maybe
import Data.SegmentTree.Strict
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import GHC.Exts
import Math.PowMod (factMods)
import GHC.Stack (HasCallStack)

-- {{{ Dictionary orders

prevPermutationVec :: (Ord e, G.Vector v e, G.Vector v (Down e)) => v e -> v e
prevPermutationVec =
  G.map (\case Down !x -> x)
    . G.modify (void . GM.nextPermutation)
    . G.map Down

-- | Returns 1-based dictionary order for the given array.
-- WARNING: Use 0-based indices for the input.
dictOrderModuloVec :: (HasCallStack, G.Vector v Int) => v Int -> Int -> Int
dictOrderModuloVec xs modulo = runST $ do
  !stree <- newSTreeVU (+) (G.length xs + 1) (0 :: Int)

  -- Pre-calculate factorial numbers:
  let !facts = factMods (G.length xs) modulo

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
  !counts <- flip G.imapM xs $ \i x -> do
    !nUsed <- fromJust <$> querySTree stree (0, x)
    let !nUnused = x - nUsed
    let !factMod = facts G.! (G.length xs - (i + 1))
    let !inc = nUnused * factMod `rem` modulo

    -- mark it as used
    insertSTree stree x 1

    return inc

  return $! succ $! G.foldl1' (\ !acc x -> (acc + x) `rem` modulo) counts

-- }}}
