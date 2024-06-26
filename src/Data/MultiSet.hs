-- | Multi set backed by `IntMap`.
module Data.MultiSet where

import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import GHC.Stack (HasCallStack)

-- | MultiSet: (nKeys, (key -> count))
type MultiSet = (Int, IM.IntMap Int)

-- | \(O(1)\)
{-# INLINE emptyMS #-}
emptyMS :: MultiSet
emptyMS = (0, IM.empty)

-- | \(O(1)\)
{-# INLINE singletonMS #-}
singletonMS :: Int -> MultiSet
singletonMS !x = (1, IM.singleton x 1)

-- | \(O(N W)\)
{-# INLINE fromListMS #-}
fromListMS :: [Int] -> MultiSet
fromListMS = foldl' (flip incMS) emptyMS

-- | \(O(W)\) Increments key.
{-# INLINE incMS #-}
incMS :: Int -> MultiSet -> MultiSet
incMS !k (!nKeys, !im) = case IM.lookup k im of
  Just !n -> (nKeys, IM.insert k (n + 1) im)
  Nothing -> (nKeys + 1, IM.insert k 1 im)

-- | \(O(W)\) Decrements key.
{-# INLINE decMS #-}
decMS :: Int -> MultiSet -> MultiSet
decMS !k (!nKeys, !im) =
  case IM.lookup k im of
    Just 1 -> (nKeys - 1, IM.delete k im)
    Just n -> (nKeys, IM.insert k (n - 1) im)
    -- TODO: prefer panic?
    Nothing -> (nKeys, im)

-- | \(O(W)\) Adds key by @dn@.
{-# INLINE addMS #-}
addMS :: Int -> Int -> MultiSet -> MultiSet
addMS !k !dn (!nKeys, !im) =
  case IM.lookup k im of
    Just n -> (nKeys, IM.insert k (n + dn) im)
    Nothing -> (nKeys + 1, IM.insert k dn im)

-- | \(O(W)\) Subtracts key by @dn@.
{-# INLINE subMS #-}
subMS :: Int -> Int -> MultiSet -> MultiSet
subMS !k !dn (!nKeys, !im) =
  case IM.lookup k im of
    Just n
      | n > dn -> (nKeys, IM.insert k (n - dn) im)
      | n == dn -> (nKeys - 1, IM.delete k im)
      -- TODO: prefer panic?
      | otherwise -> (nKeys - 1, IM.delete k im)
    Nothing -> (nKeys, im)

-- | \(O(W)\)
{-# INLINE memberMS #-}
memberMS :: Int -> MultiSet -> Bool
memberMS !k (!_, !im) = IM.member k im

-- | \(O(W)\)
{-# INLINE notMemberMS #-}
notMemberMS :: Int -> MultiSet -> Bool
notMemberMS !k (!_, !im) = IM.notMember k im

-- | \(O(W)\)
{-# INLINE decFindMinMS #-}
decFindMinMS :: MultiSet -> (Int, MultiSet)
decFindMinMS ms@(!_, !im) =
  let !key = fst $ IM.findMin im
   in (key, decMS key ms)

-- | \(O(W)\)
{-# INLINE decFindMaxMS #-}
decFindMaxMS :: MultiSet -> (Int, MultiSet)
decFindMaxMS ms@(!_, !im) =
  let !key = fst $ IM.findMax im
   in (key, decMS key ms)

-- | \(O(W)\)
{-# INLINE lookupMS #-}
lookupMS :: Int -> MultiSet -> Maybe Int
lookupMS !k = IM.lookup k . innerMS

-- | \(O(W)\) Partial alternative to `lookupMS`.
{-# INLINE getMS #-}
getMS :: (HasCallStack) => Int -> MultiSet -> Int
getMS !k !ms = case lookupMS k ms of
  Just x -> x
  Nothing -> error $ "getMS: panic with key: " ++ show k

-- | \(O(1)\) Unwraps `MultiSet` into the underlying `IntMap`.
{-# INLINE innerMS #-}
innerMS :: MultiSet -> IM.IntMap Int
innerMS (!_, !im) = im
