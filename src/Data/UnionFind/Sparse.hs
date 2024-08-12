-- | Sparse Union-Find implementation
--
-- = Typical problems
-- - [ABC 269 D - Do use hexagon grid](https://atcoder.jp/contests/abc269/tasks/abc269_d)
module Data.UnionFind.Sparse where

import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import GHC.Stack (HasCallStack)

-- | @gotoki_no_joe. Vertex -> -size | root (negative if it's root)
type SparseUnionFind = IM.IntMap Int

-- | \(O(1)\)
newSUF :: SparseUnionFind
newSUF = IM.empty

-- | \(O(1)\)
emptySUF :: SparseUnionFind
emptySUF = IM.empty

-- | \(O(\log N)\)
memberSUF :: Int -> SparseUnionFind -> Bool
memberSUF = IM.member

-- | \(O(\log N)\)
insertSUF :: Int -> SparseUnionFind -> SparseUnionFind
insertSUF !x !uf = IM.insert x (-1) uf

-- | \(O(E)\) From edges
fromListSUF :: [(Int, Int)] -> SparseUnionFind
fromListSUF = foldl' (\uf (!i, !j) -> unifySUF i j uf) newSUF

-- | \(O(E)\) From edges
fromVecSUF :: U.Vector (Int, Int) -> SparseUnionFind
fromVecSUF = U.foldl' (\uf (!i, !j) -> unifySUF i j uf) newSUF

-- | \(O(\min(N, W))\) Returns (root, size)
rootSUF :: (HasCallStack) => Int -> SparseUnionFind -> (Int, Int)
rootSUF !i !uf
  | IM.notMember i uf = (i, 1)
  | j < 0 = (i, -j)
  | otherwise = rootSUF j uf
  where
    j = uf IM.! i

-- | \(O(\min(N, W))\)
sameSUF :: (HasCallStack) => Int -> Int -> SparseUnionFind -> Bool
sameSUF !i !j !uf = fst (rootSUF i uf) == fst (rootSUF j uf)

-- | \(O(\min(N, W))\)
unifySUF :: (HasCallStack) => Int -> Int -> SparseUnionFind -> SparseUnionFind
unifySUF !i !j !uf
  | a == b = uf
  | r >= s = IM.insert a (negate $! r + s) $ IM.insert b a uf
  | otherwise = IM.insert b (negate $! r + s) $ IM.insert a b uf
  where
    (!a, !r) = rootSUF i uf
    (!b, !s) = rootSUF j uf
