module Data.Trie where

import Data.Map.Strict qualified as M
import Data.Maybe (isJust)
import Prelude hiding (lookup)

data Trie k v = Trie
  { -- keyT :: {-# UNPACK #-} Int,
    payload :: !v,
    children :: !(M.Map k (Trie k v))
  }
  deriving (Show, Eq)

-- | \(O(1)\)
root :: (Ord k) => v -> Trie k v
root x = Trie x M.empty

-- | \(O(k \log w)\)
lookup :: forall k v. (Ord k) => [k] -> Trie k v -> Maybe v
lookup = inner
  where
    inner :: [k] -> Trie k v -> Maybe v
    inner [] !trie = Just $ payload trie
    inner (k : keys) !trie = do
      child <- M.lookup k $ children trie
      inner keys child

-- | \(O(k \log w)\)
member :: forall k v. (Ord k) => [k] -> Trie k v -> Bool
member ks = isJust . lookup ks

-- | \(O(k \log w)\) Does nothing if no such vertex exist.
modify :: forall k v. (Ord k) => (v -> v) -> [k] -> Trie k v -> Trie k v
modify f = modifyNode (\(Trie p v) -> Trie (f p) v)

-- | \(O(k \log w)\) Does nothing if no such vertex exist.
modifyNode :: forall k v. (Ord k) => (Trie k v -> Trie k v) -> [k] -> Trie k v -> Trie k v
modifyNode f = inner
  where
    inner :: [k] -> Trie k v -> Trie k v
    inner [] trie = f trie
    inner (k : keys) trie = trie {children = M.adjust (inner keys) k (children trie)}

-- TODO: deduplicate the implementations of alloc*

-- | \(O(k \log w)\) Non-existing nodes in the path will have the same payload.
allocPath :: forall k v. (Ord k) => [k] -> v -> v -> Trie k v -> Trie k v
allocPath keys0 vParent vLeaf = inner keys0
  where
    inner :: [k] -> Trie k v -> Trie k v
    inner [] trie = trie
    inner (k : keys) (Trie payload children) =
      let alterF Nothing = Just $ alloc keys
          alterF (Just child) = Just $ inner keys child
       in Trie payload $! M.alter alterF k children
    alloc :: [k] -> Trie k v
    alloc [] = Trie vLeaf M.empty
    alloc (k : keys) = Trie vParent $! M.singleton k (alloc keys)

-- | \(O(k \log w)\) Allocates a path from the root to a node and modifies the payload of the
-- target node.
allocModify :: forall k v. (Ord k) => (v -> v) -> [k] -> v -> Trie k v -> Trie k v
allocModify f keys0 vDefault = inner keys0
  where
    inner :: [k] -> Trie k v -> Trie k v
    inner [] trie = trie {payload = f (payload trie)}
    inner (k : keys) (Trie payload children) =
      let alterF Nothing = Just $ alloc keys
          alterF (Just child) = Just $ inner keys child
       in Trie payload $! M.alter alterF k children
    alloc :: [k] -> Trie k v
    alloc [] = Trie (f vDefault) M.empty
    alloc (k : keys) = Trie vDefault $! M.singleton k (alloc keys)

-- | \(O(k \log w)\) Allocates a path from the root to a node and modifies the payload of the node
-- in the path.
allocModifyPath :: forall k v. (Ord k) => (v -> v) -> [k] -> v -> Trie k v -> Trie k v
allocModifyPath f keys0 vDefault = inner keys0
  where
    !fv = f vDefault
    inner :: [k] -> Trie k v -> Trie k v
    inner [] trie = trie {payload = f (payload trie)}
    inner (k : keys) (Trie payload children) =
      let alterF Nothing = Just $ alloc keys
          alterF (Just child) = Just $ inner keys child
          !payload' = f payload
       in Trie payload' $! M.alter alterF k children
    alloc :: [k] -> Trie k v
    alloc [] = Trie fv M.empty
    alloc (k : keys) = Trie fv $! M.singleton k (alloc keys)

-- | \(O(k \log w)\) Modifies payload of a node and their parents.
--
-- ==== Constraints
-- - The key must be in the map.
modifyPath :: forall k v. (Ord k) => (v -> v) -> [k] -> Trie k v -> Trie k v
modifyPath f = inner
  where
    inner :: [k] -> Trie k v -> Trie k v
    inner [] !trie = trie {payload = f (payload trie)}
    inner (k : keys) !trie = Trie (f (payload trie)) (M.adjust (inner keys) k (children trie))

-- | \(O(k \log w)\) Returns prefix payloads, excluding the root.
path :: forall k v. (Ord k) => [k] -> Trie k v -> [v]
path = inner
  where
    inner :: [k] -> Trie k v -> [v]
    inner [] !trie = []
    inner (k : keys) !trie = do
      -- TODO: write monadic code
      case M.lookup k (children trie) of
        Just child -> (payload child :) $ inner keys child
        Nothing -> []
