-- | Multi set backed by `Map`.
module Data.MultiSet2 where

import Data.List (foldl')
import qualified Data.Map.Strict as M
import GHC.Stack (HasCallStack)

-- | MultiSet2: (nKeys, (key -> count))
type MultiSet2 k = (Int, M.Map k Int)

-- | \(O(1)\)
{-# INLINE emptyMS2 #-}
emptyMS2 :: MultiSet2 k
emptyMS2 = (0, M.empty)

-- | \(O(1)\)
{-# INLINE singletonMS2 #-}
singletonMS2 :: k -> MultiSet2 k
singletonMS2 !x = (1, M.singleton x 1)

-- | \(O(N W)\)
{-# INLINE fromListMS2 #-}
fromListMS2 :: (Ord k) => [k] -> MultiSet2 k
fromListMS2 = foldl' (flip incMS2) emptyMS2

-- | \(O(W)\) Increments key.
{-# INLINE incMS2 #-}
incMS2 :: (Ord k) => k -> MultiSet2 k -> MultiSet2 k
incMS2 !k (!nKeys, !im) = case M.lookup k im of
  Just !n -> (nKeys, M.insert k (n + 1) im)
  Nothing -> (nKeys + 1, M.insert k 1 im)

-- | \(O(W)\) Decrements key.
{-# INLINE decMS2 #-}
decMS2 :: (Ord k) => k -> MultiSet2 k -> MultiSet2 k
decMS2 !k (!nKeys, !im) =
  case M.lookup k im of
    Just 1 -> (nKeys - 1, M.delete k im)
    Just n -> (nKeys, M.insert k (n - 1) im)
    -- TODO: prefer panic?
    Nothing -> (nKeys, im)

-- | \(O(W)\) Adds key by @dn@.
{-# INLINE addMS2 #-}
addMS2 :: (Ord k) => k -> Int -> MultiSet2 k -> MultiSet2 k
addMS2 !k !dn (!nKeys, !im) =
  case M.lookup k im of
    Just n -> (nKeys, M.insert k (n + dn) im)
    Nothing -> (nKeys + 1, M.insert k dn im)

-- | \(O(W)\) Subtracts key by @dn@.
{-# INLINE subMS2 #-}
subMS2 :: (Ord k) => k -> Int -> MultiSet2 k -> MultiSet2 k
subMS2 !k !dn (!nKeys, !im) =
  case M.lookup k im of
    Just n
      | n > dn -> (nKeys, M.insert k (n - dn) im)
      | n == dn -> (nKeys - 1, M.delete k im)
      -- TODO: prefer panic?
      | otherwise -> (nKeys - 1, M.delete k im)
    Nothing -> (nKeys, im)

-- | \(O(W)\)
{-# INLINE memberMS2 #-}
memberMS2 :: (Ord k) => k -> MultiSet2 k -> Bool
memberMS2 !k (!_, !im) = M.member k im

-- | \(O(W)\)
{-# INLINE notMemberMS2 #-}
notMemberMS2 :: (Ord k) => k -> MultiSet2 k -> Bool
notMemberMS2 !k (!_, !im) = M.notMember k im

-- | \(O(W)\)
{-# INLINE decFindMinMS2 #-}
decFindMinMS2 :: (Ord k) => MultiSet2 k -> (k, MultiSet2 k)
decFindMinMS2 ms@(!_, !im) =
  let !key = fst $ M.findMin im
   in (key, decMS2 key ms)

-- | \(O(W)\)
{-# INLINE decFindMaxMS2 #-}
decFindMaxMS2 :: (Ord k) => MultiSet2 k -> (k, MultiSet2 k)
decFindMaxMS2 ms@(!_, !im) =
  let !key = fst $ M.findMax im
   in (key, decMS2 key ms)

-- | \(O(W)\)
{-# INLINE lookupMS2 #-}
lookupMS2 :: (Ord k) => k -> MultiSet2 k -> Maybe Int
lookupMS2 !k = M.lookup k . innerMS2

-- | \(O(W)\) Partial alternative to `lookupMS2`.
{-# INLINE getMS2 #-}
getMS2 :: (HasCallStack, Ord k, Show k) => k -> MultiSet2 k -> Int
getMS2 !k !ms = case lookupMS2 k ms of
  Just x -> x
  Nothing -> error $ "getMS2: panic with key: " ++ show k

-- | \(O(1)\) Unwraps `MultiSet2` into the underlying `Map`.
{-# INLINE innerMS2 #-}
innerMS2 :: MultiSet2 k -> M.Map k Int
innerMS2 (!_, !im) = im
