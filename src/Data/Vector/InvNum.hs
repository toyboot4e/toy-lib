-- | Inversion numbers.
module Data.Vector.InvNum where

import Control.Monad.ST
import Data.Maybe
import Data.Monoid
import Data.SegmentTree.Strict
import Data.Vector.Extra (compressU)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import GHC.Stack (HasCallStack)
import Math.PowMod (factModsN)

-- | \(O(N \log N)\) Calculates the inversion number. Be sure to compress the input vector!
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

-- | \(O(N \log N)\) Calculates the inversion number, but after applying index compression.
-- It can significantly improve the performance, like in ABC 261 F.
compressInvNumG :: (HasCallStack) => U.Vector Int -> Int
compressInvNumG xs = invNum (U.length xs' - 1) xs'
  where
    !xs' = snd $ compressU xs

-- | \(O(N \log N)\) Returns 1-based lexicographic order of the given array.
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
