module Data.UnionFind.Sparse where

import Data.List (foldl')
import qualified Data.IntMap.Strict as IM
import GHC.Stack (HasCallStack)

-- {{{ Sparse, immutable union-find tree

-- @gotoki_no_joe
type SparseUnionFind = IM.IntMap Int

newSUF :: SparseUnionFind
newSUF = IM.empty

-- from edges
fromListSUF :: [(Int, Int)] -> SparseUnionFind
fromListSUF = foldl' (uncurry . uniteSUF) newSUF

rootSUF :: HasCallStack => SparseUnionFind -> Int -> (Int, Int)
rootSUF !uf !i
  | IM.notMember i uf = (i, 1)
  | j < 0 = (i, -j)
  | otherwise = rootSUF uf j
  where
    j = uf IM.! i

sameSUF :: HasCallStack => SparseUnionFind -> Int -> Int -> Bool
sameSUF !uf !i !j = fst (rootSUF uf i) == fst (rootSUF uf j)

uniteSUF :: HasCallStack => SparseUnionFind -> Int -> Int -> SparseUnionFind
uniteSUF !uf !i !j
  | a == b = uf
  | r >= s = IM.insert a (negate $! r + s) $ IM.insert b a uf
  | otherwise = IM.insert b (negate $! r + s) $ IM.insert a b uf
  where
    (!a, !r) = rootSUF uf i
    (!b, !s) = rootSUF uf j

-- }}}
