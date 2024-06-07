-- | @IntervalSet@ is a sparse set that manages non-overlapping @(l, r)@ value pairs.
--
-- Typically used with @StateT@.
--
-- = Typical problems
-- - [ABC 330 E - Mex and Update](https://atcoder.jp/contests/abc330/tasks/abc330_e)
module Data.IntervalSet where

import Control.Monad.Identity (runIdentity)
import qualified Data.IntMap.Strict as IM
import Data.IntervalMap

-- | @IntervalSet@ is a sparse set that manages non-overlapping @(l, r)@ value pairs.
type IntervalSet = IntervalMap ()

-- | \(O(1)\) Creates an empty `IntervalSet`.
{-# INLINE emptyIS #-}
emptyIS :: IntervalSet
emptyIS = IntervalMap IM.empty

-- | \(O(\min(n, W))\) Looks up an interval that contains @[l, r]@.
{-# INLINE lookupIS #-}
lookupIS :: Int -> Int -> IntervalSet -> Maybe (Int, Int)
lookupIS l r rm = (\(!l', !r', !_) -> (l', r')) <$> lookupIM l r rm

-- | \(O(\min(n, W))\) Boolean variant of `lookupIM`.
{-# INLINE intersectsIS #-}
intersectsIS :: Int -> Int -> IntervalSet -> Bool
intersectsIS = intersectsIM

-- | \(O(\min(n, W))\) Point variant of `intersectsIM`.
{-# INLINE containsIS #-}
containsIS :: Int -> IntervalSet -> Bool
containsIS = containsIM

-- | Amortized \(O(\min(\log n, W))\) interval insertion with side effects. Old overlapping
-- intervals are overwritten.
{-# INLINE insertMIS #-}
insertMIS :: (Monad m) => Int -> Int -> (Int -> Int -> m ()) -> (Int -> Int -> m ()) -> IntervalSet -> m IntervalSet
insertMIS l r onAdd onDel = insertMIM l r () onAdd' onDel'
  where
    onAdd' l' r' () = onAdd l' r'
    onDel' l' r' () = onDel l' r'

-- | Amortized \(O(\min(\log n, W))\) interval insertion. Old overlapping intervals are overwritten.
{-# INLINE insertIS #-}
insertIS :: Int -> Int -> IntervalSet -> IntervalSet
insertIS l r = insertIM l r ()

-- | Amortized \(O(\min(\log n, W))\) interval deletion with side effects.
{-# INLINE deleteMIS #-}
deleteMIS :: (Monad m) => Int -> Int -> (Int -> Int -> m ()) -> IntervalSet -> m IntervalSet
deleteMIS l0 r0 onDel = deleteMIM l0 r0 onDel'
  where
    onDel' l' r' () = onDel l' r'

-- | Amortized \(O(\min(\log n, W))\) interval deletion.
{-# INLINE deleteIS #-}
deleteIS :: Int -> Int -> IntervalSet -> IntervalSet
deleteIS l r = runIdentity . deleteMIM l r onDel
  where
    onDel _ _ () = pure ()

-- | \(O(\min(\log n, W))\) Mex retrieval.
{-# INLINE mexIS #-}
mexIS :: IntervalSet -> Int
mexIS = mexIM
