module Data.Trie where

import qualified Data.Map.Strict as M
import Data.Maybe (isJust)

data Trie k v = Trie
  { -- keyT :: {-# UNPACK #-} Int,
    payloadT :: !v,
    childrenT :: !(M.Map k (Trie k v))
  }
  deriving (Show, Eq)

-- | \(O(1)\)
rootT :: (Ord k) => v -> Trie k v
rootT x = Trie x M.empty

-- | \(O(k \log w)\)
lookupT :: forall k v. (Ord k) => [k] -> Trie k v -> Maybe v
lookupT = inner
  where
    inner :: [k] -> Trie k v -> Maybe v
    inner [] !trie = Just $ payloadT trie
    inner (k : keys) !trie = do
      child <- M.lookup k $ childrenT trie
      inner keys child

-- | \(O(k \log w)\)
memberT :: forall k v. (Ord k) => [k] -> Trie k v -> Bool
memberT ks = isJust . lookupT ks

-- | \(O(k \log w)\) Does nothing if no such vertex exist.
modifyT :: forall k v. (Ord k) => (v -> v) -> [k] -> Trie k v -> Trie k v
modifyT f = modifyNodeT (\(Trie p v) -> Trie (f p) v)

-- | \(O(k \log w)\) Does nothing if no such vertex exist.
modifyNodeT :: forall k v. (Ord k) => (Trie k v -> Trie k v) -> [k] -> Trie k v -> Trie k v
modifyNodeT f = inner
  where
    inner :: [k] -> Trie k v -> Trie k v
    inner [] trie = f trie
    inner (k : keys) trie = trie {childrenT = M.adjust (inner keys) k (childrenT trie)}

-- TODO: deduplicate the implementations of alloc*

-- | \(O(k \log w)\) Non-existing nodes in the path will have the same payload.
allocPathT :: forall k v. (Ord k) => [k] -> v -> v -> Trie k v -> Trie k v
allocPathT keys0 vParent vLeaf = inner keys0
  where
    inner :: [k] -> Trie k v -> Trie k v
    inner [] trie = trie
    inner (k : keys) (Trie payload children) =
      let alterF Nothing = Just $ allocPath keys
          alterF (Just child) = Just $ inner keys child
       in Trie payload $! M.alter alterF k children
    allocPath :: [k] -> Trie k v
    allocPath [] = Trie vLeaf M.empty
    allocPath (k : keys) = Trie vParent $! M.singleton k (allocPath keys)

-- | \(O(k \log w)\) Allocates a path from the root to a node and modifies the payload of the
-- target node.
allocModifyT :: forall k v. (Ord k) => (v -> v) -> [k] -> v -> Trie k v -> Trie k v
allocModifyT f keys0 vDefault = inner keys0
  where
    inner :: [k] -> Trie k v -> Trie k v
    inner [] trie = trie {payloadT = f (payloadT trie)}
    inner (k : keys) (Trie payload children) =
      let alterF Nothing = Just $ allocPath keys
          alterF (Just child) = Just $ inner keys child
       in Trie payload $! M.alter alterF k children
    allocPath :: [k] -> Trie k v
    allocPath [] = Trie (f vDefault) M.empty
    allocPath (k : keys) = Trie vDefault $! M.singleton k (allocPath keys)

-- | \(O(k \log w)\) Allocates a path from the root to a node and modifies the payload of the node
-- in the path.
allocModifyPathT :: forall k v. (Ord k) => (v -> v) -> [k] -> v -> Trie k v -> Trie k v
allocModifyPathT f keys0 vDefault = inner keys0
  where
    !fv = f vDefault
    inner :: [k] -> Trie k v -> Trie k v
    inner [] trie = trie {payloadT = f (payloadT trie)}
    inner (k : keys) (Trie payload children) =
      let alterF Nothing = Just $ allocPath keys
          alterF (Just child) = Just $ inner keys child
          !payload' = f payload
       in Trie payload' $! M.alter alterF k children
    allocPath :: [k] -> Trie k v
    allocPath [] = Trie fv M.empty
    allocPath (k : keys) = Trie fv $! M.singleton k (allocPath keys)

-- | \(O(k \log w)\) Modifies payload of a node and their parents.
--
-- ==== Constraints
-- - The key must be in the map.
modifyPathT :: forall k v. (Ord k) => (v -> v) -> [k] -> Trie k v -> Trie k v
modifyPathT f = inner
  where
    inner :: [k] -> Trie k v -> Trie k v
    inner [] !trie = trie {payloadT = f (payloadT trie)}
    inner (k : keys) !trie = Trie (f (payloadT trie)) (M.adjust (inner keys) k (childrenT trie))

-- | \(O(k \log w)\) Returns prefix payloads, excluding the root.
pathT :: forall k v. (Ord k) => [k] -> Trie k v -> [v]
pathT = inner
  where
    inner :: [k] -> Trie k v -> [v]
    inner [] !trie = []
    inner (k : keys) !trie = do
      -- TODO: write monadic code
      case M.lookup k (childrenT trie) of
        Just child -> (payloadT child :) $ inner keys child
        Nothing -> []
