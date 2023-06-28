{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

-- | Strongly connected components and topological sort.
module Data.Graph.Scc where

import Control.Monad
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST
import Data.Array.IArray
import Data.List (foldl')
import qualified Data.Vector.Unboxed.Mutable as VUM

-- {{{ Topological sort / SCC

-- | Topological sort implemented with postorder DFS.
--
-- = Implementation note
-- Topological sort is for DAG, but internally it's used for `scc` where cyclic graph input can
-- come.
topSort :: Array Int [Int] -> [Int]
topSort !graph = runST $ do
  let !bounds_ = bounds graph
  !vis <- VUM.replicate (succ $ rangeSize bounds_) False

  let dfsM !acc !v = do
        !b <- VUM.unsafeRead vis (index bounds_ v)
        if b
          then return acc
          else do
            VUM.unsafeWrite vis (index bounds_ v) True
            !vs <- filterM (fmap not . VUM.unsafeRead vis . index bounds_) $ graph ! v
            -- Create postorder output:
            (v :) <$> foldM dfsM acc vs

  foldM dfsM [] $ range bounds_

-- | Partial running of `scc` over topologically sorted vertices, but for some connected components
-- only.
topScc1 :: forall m. (PrimMonad m) => Array Int [Int] -> VUM.MVector (PrimState m) Bool -> Int -> m [Int]
topScc1 !graph' !vis !v0 = do
  let !bounds_ = bounds graph'

  let dfsM !acc !v = do
        !b <- VUM.unsafeRead vis (index bounds_ v)
        if b
          then return acc
          else do
            VUM.unsafeWrite vis (index bounds_ v) True
            !vs <- filterM (fmap not . VUM.unsafeRead vis . index bounds_) $ graph' ! v
            -- Create preorder output:
            (v :) <$> foldM dfsM acc vs

  dfsM [] v0

-- | Retrieves a reverse graph
revGraph :: Array Int [Int] -> Array Int [Int]
revGraph graph = accumArray (flip (:)) [] (bounds graph) input
  where
    input :: [(Int, Int)]
    input = foldl' (\ !acc (!v2, !v1s) -> foldl' (\ !acc' !v1 -> (v1, v2) : acc') acc v1s) [] $ assocs graph

-- | Collectes strongly connected components, topologically sorted.
-- Upstream vertices come first, e.g., @(v1 - v2) -> v3 -> v4@.
topScc :: Array Int [Int] -> [[Int]]
topScc graph = collectSccPreorder $ topSort graph
  where
    graph' = revGraph graph

    collectSccPreorder :: [Int] -> [[Int]]
    collectSccPreorder !topVerts = runST $ do
      let !bounds_ = bounds graph'
      !vis <- VUM.replicate (succ $ rangeSize bounds_) False
      filter (not . null) <$> mapM (topScc1 graph' vis) topVerts

-- | Collects cycles using `scc`.
topSccCycles :: Array Int [Int] -> [[Int]]
topSccCycles graph = filter f $ topScc graph
  where
    -- self-referencial loop only
    f [!v] = [v] == graph ! v
    f !_ = True

-- | Collectes strongly connected components, reverse topologically sorted.
-- Down stream vertices come first, e.g., @v4 <- v3 <- (v2 - v1)@
downScc :: Array Int [Int] -> [[Int]]
downScc = reverse . map reverse . topScc

-- }}}
