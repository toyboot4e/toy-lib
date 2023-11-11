module Data.UnionFind.Sparse where

-- | Sparse Union-Find implementation
--
-- = Typical problems
-- - [ABC 269 D - Do use hexagon grid](https://atcoder.jp/contests/abc269/tasks/abc269_d)

import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import GHC.Stack (HasCallStack)

-- {{{ Sparse, immutable union-find tree

-- | @gotoki_no_joe. Vertex -> size + root? (negative if it's root)
type SparseUnionFind = IM.IntMap Int

newSUF :: SparseUnionFind
newSUF = IM.empty

memberSUF :: Int -> SparseUnionFind -> Bool
memberSUF = IM.member

insertSUF :: Int -> SparseUnionFind -> SparseUnionFind
insertSUF !x !uf = IM.insert x (-1) uf

-- | From edges
fromListSUF :: [(Int, Int)] -> SparseUnionFind
fromListSUF = foldl' (uncurry . uniteSUF) newSUF

-- | From edges
fromVecSUF :: U.Vector (Int, Int) -> SparseUnionFind
fromVecSUF = U.foldl' (uncurry . uniteSUF) newSUF

-- | Returns (root, size)
rootSUF :: (HasCallStack) => SparseUnionFind -> Int -> (Int, Int)
rootSUF !uf !i
  | IM.notMember i uf = (i, 1)
  | j < 0 = (i, -j)
  | otherwise = rootSUF uf j
  where
    j = uf IM.! i

sameSUF :: (HasCallStack) => SparseUnionFind -> Int -> Int -> Bool
sameSUF !uf !i !j = fst (rootSUF uf i) == fst (rootSUF uf j)

uniteSUF :: (HasCallStack) => SparseUnionFind -> Int -> Int -> SparseUnionFind
uniteSUF !uf !i !j
  | a == b = uf
  | r >= s = IM.insert a (negate $! r + s) $ IM.insert b a uf
  | otherwise = IM.insert b (negate $! r + s) $ IM.insert a b uf
  where
    (!a, !r) = rootSUF uf i
    (!b, !s) = rootSUF uf j

uniteSUF2 :: HasCallStack => Int -> Int -> SparseUnionFind -> SparseUnionFind
uniteSUF2 !i !j !uf = uniteSUF uf i j

-- }}}
