-- | Interval set.
--
-- = Typical problems
-- - [ABC 330 E - Mex and Update](https://atcoder.jp/contests/abc330/tasks/abc330_e)
module Data.IntervalSet where

import Control.Monad.Identity (runIdentity)
import qualified Data.IntMap.Strict as IM
import Data.IntervalMap

type IntervalSet = IntervalMap ()

{-# INLINE emptyIS #-}
emptyIS :: IntervalSet
emptyIS = IntervalMap IM.empty

{-# INLINE lookupIS #-}
lookupIS :: Int -> Int -> IntervalSet -> Maybe (Int, Int)
lookupIS l r rm = (\(!l', !r', !_) -> (l', r')) <$> lookupIM l r rm

-- | Boolean variant of `lookupIM`.
{-# INLINE intersectsIS #-}
intersectsIS :: Int -> Int -> IntervalSet -> Bool
intersectsIS = intersectsIM

-- | Point variant of `intersectsIM`.
{-# INLINE containsIS #-}
containsIS :: Int -> IntervalSet -> Bool
containsIS = containsIM

{-# INLINE insertMIS #-}
insertMIS :: (Monad m) => Int -> Int -> (Int -> Int -> m ()) -> (Int -> Int -> m ()) -> IntervalSet -> m IntervalSet
insertMIS l r onAdd onDel = insertMIM l r () onAdd' onDel'
  where
    onAdd' l' r' () = onAdd l' r'
    onDel' l' r' () = onDel l' r'

{-# INLINE insertIS #-}
insertIS :: Int -> Int -> IntervalSet -> IntervalSet
insertIS l r = insertIM l r ()

{-# INLINE deleteMIS #-}
deleteMIS :: (Monad m) => Int -> Int -> (Int -> Int -> m ()) -> IntervalSet -> m IntervalSet
deleteMIS l0 r0 onDel = deleteMIM l0 r0 onDel'
  where
    onDel' l' r' () = onDel l' r'

{-# INLINE deleteIS #-}
deleteIS :: Int -> Int -> IntervalSet -> IntervalSet
deleteIS l r = runIdentity . deleteMIM l r onDel
  where
    onDel _ _ () = pure ()

-- | REMARK: The interval map has to be like a set.
{-# INLINE mexIS #-}
mexIS :: IntervalSet -> Int
mexIS = mexIM
