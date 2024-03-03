-- | Multi set backed by `IntMap`.
module Data.MultiSet where

import qualified Data.IntMap.Strict as IM
import Data.List (foldl')

-- | MultiSet: (nKeys, (key -> count))
type MultiSet = (Int, IM.IntMap Int)

{-# INLINE emptyMS #-}
emptyMS :: MultiSet
emptyMS = (0, IM.empty)

{-# INLINE singletonMS #-}
singletonMS :: Int -> MultiSet
singletonMS !x = (1, IM.singleton x 1)

{-# INLINE fromListMS #-}
fromListMS :: [Int] -> MultiSet
fromListMS = foldl' (flip incMS) emptyMS

-- | Increments key.
{-# INLINE incMS #-}
incMS :: Int -> MultiSet -> MultiSet
incMS !k (!n, !im) =
  if IM.member k im
    then (n, IM.insertWith (+) k 1 im)
    else (n + 1, IM.insert k 1 im)

-- | Decrements key. Key with count zero are removed.
{-# INLINE incMS #-}
decMS :: Int -> MultiSet -> MultiSet
decMS !k (!n, !im) =
  case IM.lookup k im of
    Just 1 -> (n - 1, IM.delete k im)
    Just _ -> (n, IM.insertWith (+) k (-1) im)
    Nothing -> (n, im)

{-# INLINE memberMS #-}
memberMS :: Int -> MultiSet -> Bool
memberMS !k (!_, !im) = IM.member k im

{-# INLINE notMemberMS #-}
notMemberMS :: Int -> MultiSet -> Bool
notMemberMS !k (!_, !im) = IM.notMember k im

{-# INLINE decFindMinMS #-}
decFindMinMS :: MultiSet -> (Int, MultiSet)
decFindMinMS ms@(!_, !im) =
  let !key = fst $ IM.findMin im
   in (key, decMS key ms)

-- | Unwraps `MultiSet` into the underlying `IntMap`.
{-# INLINE innerMS #-}
innerMS :: MultiSet -> IM.IntMap Int
innerMS (!_, !im) = im

