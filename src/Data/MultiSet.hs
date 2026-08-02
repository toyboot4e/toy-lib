-- | Multi set backed by `IntMap`.
module Data.MultiSet where

import Data.IntMap.Strict qualified as IM
import Data.List (foldl')
import GHC.Stack (HasCallStack)
import Prelude hiding (lookup)

-- | MultiSet: (nKeys, (key -> count))
type MultiSet = (Int, IM.IntMap Int)

-- | \(O(1)\)
{-# INLINE empty #-}
empty :: MultiSet
empty = (0, IM.empty)

-- | \(O(1)\)
{-# INLINE singleton #-}
singleton :: Int -> MultiSet
singleton !x = (1, IM.singleton x 1)

-- | \(O(N W)\)
{-# INLINE fromList #-}
fromList :: [Int] -> MultiSet
fromList = foldl' (flip inc) empty

-- | \(O(W)\) Increments key.
{-# INLINE inc #-}
inc :: Int -> MultiSet -> MultiSet
inc !k (!nKeys, !im) = case IM.lookup k im of
  Just !n -> (nKeys, IM.insert k (n + 1) im)
  Nothing -> (nKeys + 1, IM.insert k 1 im)

-- | \(O(W)\) Decrements key.
{-# INLINE dec #-}
dec :: Int -> MultiSet -> MultiSet
dec !k (!nKeys, !im) =
  case IM.lookup k im of
    Just 1 -> (nKeys - 1, IM.delete k im)
    Just n -> (nKeys, IM.insert k (n - 1) im)
    -- TODO: prefer panic?
    Nothing -> (nKeys, im)

-- | \(O(W)\) Adds key by @dn@.
{-# INLINE add #-}
add :: Int -> Int -> MultiSet -> MultiSet
add !k !dn (!nKeys, !im) =
  case IM.lookup k im of
    Just n -> (nKeys, IM.insert k (n + dn) im)
    Nothing -> (nKeys + 1, IM.insert k dn im)

-- | \(O(W)\) Subtracts key by @dn@.
{-# INLINE sub #-}
sub :: Int -> Int -> MultiSet -> MultiSet
sub !k !dn (!nKeys, !im) =
  case IM.lookup k im of
    Just n
      | n > dn -> (nKeys, IM.insert k (n - dn) im)
      | n == dn -> (nKeys - 1, IM.delete k im)
      -- TODO: prefer panic?
      | otherwise -> (nKeys - 1, IM.delete k im)
    Nothing -> (nKeys, im)

-- | \(O(W)\)
{-# INLINE member #-}
member :: Int -> MultiSet -> Bool
member !k (!_, !im) = IM.member k im

-- | \(O(W)\)
{-# INLINE notMember #-}
notMember :: Int -> MultiSet -> Bool
notMember !k (!_, !im) = IM.notMember k im

-- | \(O(W)\)
{-# INLINE decFindMin #-}
decFindMin :: MultiSet -> (Int, MultiSet)
decFindMin ms@(!_, !im) =
  let !key = fst $ IM.findMin im
   in (key, dec key ms)

-- | \(O(W)\)
{-# INLINE decFindMax #-}
decFindMax :: MultiSet -> (Int, MultiSet)
decFindMax ms@(!_, !im) =
  let !key = fst $ IM.findMax im
   in (key, dec key ms)

-- | \(O(W)\)
{-# INLINE lookup #-}
lookup :: Int -> MultiSet -> Maybe Int
lookup !k = IM.lookup k . inner

-- | \(O(W)\) Partial alternative to `lookup`.
{-# INLINE get #-}
get :: (HasCallStack) => Int -> MultiSet -> Int
get !k !ms = case lookup k ms of
  Just x -> x
  Nothing -> error $ "get: panic with key: " ++ show k

-- | \(O(1)\) Unwraps `MultiSet` into the underlying `IntMap`.
{-# INLINE inner #-}
inner :: MultiSet -> IM.IntMap Int
inner (!_, !im) = im
