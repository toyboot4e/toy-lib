-- | Sparse Union-Find implementation
--
-- = Typical problems
-- - [ABC 269 D - Do use hexagon grid](https://atcoder.jp/contests/abc269/tasks/abc269_d)
module Data.UnionFind.Sparse where

import Data.IntMap.Strict qualified as IM
import Data.List (foldl')
import Data.Vector.Unboxed qualified as U
import GHC.Stack (HasCallStack)

-- | @gotoki_no_joe. Vertex -> -size | root (negative if it's root)
type SparseUnionFind = IM.IntMap Int

-- | \(O(1)\)
new :: SparseUnionFind
new = IM.empty

-- | \(O(1)\)
empty :: SparseUnionFind
empty = IM.empty

-- | \(O(\log N)\)
member :: Int -> SparseUnionFind -> Bool
member = IM.member

-- | \(O(\log N)\)
insert :: Int -> SparseUnionFind -> SparseUnionFind
insert !x !uf = IM.insert x (-1) uf

-- | \(O(E)\) From edges
fromList :: [(Int, Int)] -> SparseUnionFind
fromList = foldl' (\uf (!i, !j) -> unify i j uf) new

-- | \(O(E)\) From edges
fromVec :: U.Vector (Int, Int) -> SparseUnionFind
fromVec = U.foldl' (\uf (!i, !j) -> unify i j uf) new

-- | \(O(\min(N, W))\) Returns (root, size)
root :: (HasCallStack) => Int -> SparseUnionFind -> (Int, Int)
root !i !uf
  | IM.notMember i uf = (i, 1)
  | j < 0 = (i, -j)
  | otherwise = root j uf
  where
    j = uf IM.! i

-- | \(O(\min(N, W))\)
same :: (HasCallStack) => Int -> Int -> SparseUnionFind -> Bool
same !i !j !uf = fst (root i uf) == fst (root j uf)

-- | \(O(\min(N, W))\)
unify :: (HasCallStack) => Int -> Int -> SparseUnionFind -> SparseUnionFind
unify !i !j !uf
  | a == b = uf
  | r >= s = IM.insert a (negate $! r + s) $ IM.insert b a uf
  | otherwise = IM.insert b (negate $! r + s) $ IM.insert a b uf
  where
    (!a, !r) = root i uf
    (!b, !s) = root j uf
