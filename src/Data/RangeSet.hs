-- | Range set.
--
-- = Typical problems
-- - [ABC 330 E - Mex and Update](https://atcoder.jp/contests/abc330/tasks/abc330_e)
module Data.RangeSet where

import Control.Monad.Identity (runIdentity)
import qualified Data.IntMap.Strict as IM
import Data.RangeMap

type RangeSet = RangeMap ()

{-# INLINE emptyRS #-}
emptyRS :: RangeSet
emptyRS = RangeMap IM.empty

{-# INLINE lookupRS #-}
lookupRS :: Int -> Int -> RangeSet -> Maybe (Int, Int)
lookupRS l r rm = (\(!l', !r', !_) -> (l', r')) <$> lookupRM l r rm

-- | Boolean variant of `lookupRM`.
{-# INLINE intersectsRS #-}
intersectsRS :: Int -> Int -> RangeSet -> Bool
intersectsRS = intersectsRM

-- | Point variant of `intersectsRM`.
{-# INLINE containsRS #-}
containsRS :: Int -> RangeSet -> Bool
containsRS = containsRM

{-# INLINE insertMRS #-}
insertMRS :: (Monad m) => Int -> Int -> (Int -> Int -> m ()) -> (Int -> Int -> m ()) -> RangeSet -> m RangeSet
insertMRS l r onAdd onDel = insertMRM l r () onAdd' onDel'
  where
    onAdd' l' r' () = onAdd l' r'
    onDel' l' r' () = onDel l' r'

{-# INLINE insertRS #-}
insertRS :: Int -> Int -> RangeSet -> RangeSet
insertRS l r = insertRM l r ()

{-# INLINE deleteMRS #-}
deleteMRS :: (Monad m) => Int -> Int -> (Int -> Int -> m ()) -> RangeSet -> m RangeSet
deleteMRS l0 r0 onDel = deleteMRM l0 r0 onDel'
  where
    onDel' l' r' () = onDel l' r'

{-# INLINE deleteRS #-}
deleteRS :: Int -> Int -> RangeSet -> RangeSet
deleteRS l r = runIdentity . deleteMRM l r onDel
  where
    onDel _ _ () = pure ()

-- | REMARK: The range map has to be like a set.
{-# INLINE mexRS #-}
mexRS :: RangeSet -> Int
mexRS = mexRM
