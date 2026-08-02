-- | Multi set backed by `Map`.
module Data.MultiSet2 where

import Data.List (foldl')
import Data.Map.Strict qualified as M
import GHC.Stack (HasCallStack)
import Prelude hiding (lookup)

-- | MultiSet2: (nKeys, (key -> count))
type MultiSet2 k = (Int, M.Map k Int)

-- | \(O(1)\)
{-# INLINE empty #-}
empty :: MultiSet2 k
empty = (0, M.empty)

-- | \(O(1)\)
{-# INLINE singleton #-}
singleton :: k -> MultiSet2 k
singleton !x = (1, M.singleton x 1)

-- | \(O(N W)\)
{-# INLINE fromList #-}
fromList :: (Ord k) => [k] -> MultiSet2 k
fromList = foldl' (flip inc) empty

-- | \(O(W)\) Increments key.
{-# INLINE inc #-}
inc :: (Ord k) => k -> MultiSet2 k -> MultiSet2 k
inc !k (!nKeys, !im) = case M.lookup k im of
  Just !n -> (nKeys, M.insert k (n + 1) im)
  Nothing -> (nKeys + 1, M.insert k 1 im)

-- | \(O(W)\) Decrements key.
{-# INLINE dec #-}
dec :: (Ord k) => k -> MultiSet2 k -> MultiSet2 k
dec !k (!nKeys, !im) =
  case M.lookup k im of
    Just 1 -> (nKeys - 1, M.delete k im)
    Just n -> (nKeys, M.insert k (n - 1) im)
    -- TODO: prefer panic?
    Nothing -> (nKeys, im)

-- | \(O(W)\) Adds key by @dn@.
{-# INLINE add #-}
add :: (Ord k) => k -> Int -> MultiSet2 k -> MultiSet2 k
add !k !dn (!nKeys, !im) =
  case M.lookup k im of
    Just n -> (nKeys, M.insert k (n + dn) im)
    Nothing -> (nKeys + 1, M.insert k dn im)

-- | \(O(W)\) Subtracts key by @dn@.
{-# INLINE sub #-}
sub :: (Ord k) => k -> Int -> MultiSet2 k -> MultiSet2 k
sub !k !dn (!nKeys, !im) =
  case M.lookup k im of
    Just n
      | n > dn -> (nKeys, M.insert k (n - dn) im)
      | n == dn -> (nKeys - 1, M.delete k im)
      -- TODO: prefer panic?
      | otherwise -> (nKeys - 1, M.delete k im)
    Nothing -> (nKeys, im)

-- | \(O(W)\)
{-# INLINE member #-}
member :: (Ord k) => k -> MultiSet2 k -> Bool
member !k (!_, !im) = M.member k im

-- | \(O(W)\)
{-# INLINE notMember #-}
notMember :: (Ord k) => k -> MultiSet2 k -> Bool
notMember !k (!_, !im) = M.notMember k im

-- | \(O(W)\)
{-# INLINE decFindMin #-}
decFindMin :: (Ord k) => MultiSet2 k -> (k, MultiSet2 k)
decFindMin ms@(!_, !im) =
  let !key = fst $ M.findMin im
   in (key, dec key ms)

-- | \(O(W)\)
{-# INLINE decFindMax #-}
decFindMax :: (Ord k) => MultiSet2 k -> (k, MultiSet2 k)
decFindMax ms@(!_, !im) =
  let !key = fst $ M.findMax im
   in (key, dec key ms)

-- | \(O(W)\)
{-# INLINE lookup #-}
lookup :: (Ord k) => k -> MultiSet2 k -> Maybe Int
lookup !k = M.lookup k . inner

-- | \(O(W)\) Partial alternative to `lookup`.
{-# INLINE get #-}
get :: (HasCallStack, Ord k, Show k) => k -> MultiSet2 k -> Int
get !k !ms = case lookup k ms of
  Just x -> x
  Nothing -> error $ "get: panic with key: " ++ show k

-- | \(O(1)\) Unwraps `MultiSet2` into the underlying `Map`.
{-# INLINE inner #-}
inner :: MultiSet2 k -> M.Map k Int
inner (!_, !im) = im
