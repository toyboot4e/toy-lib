-- | Tree folding.

module Data.Tree.Fold where

import Control.Monad
import Control.Monad.Fix
import Data.Array.IArray
import Data.Graph (Vertex)
import Data.List (foldl')
import Data.SemigroupAction
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Stack (HasCallStack)

-- {{{ Tree folding from a root node

-- | Folds a tree from one root vertex using postorder DFS.
foldTree :: forall m a. Array Vertex [Vertex] -> Vertex -> (m -> a -> a) -> (Vertex -> a) -> (a -> m) -> a
foldTree !tree !root !sact !acc0At !toM = inner (-1) root
  where
    inner :: Vertex -> Vertex -> a
    inner !parent !v1 =
      let !v2s = filter (/= parent) $ tree ! v1
       in foldl' (\acc v2 -> (toM $! inner v1 v2) `sact` acc) (acc0At v1) v2s

-- | Folds a tree from one root vertex using postorder DFS, recording all the accumulation values
-- on every vertex.
scanTreeVG :: (HasCallStack, VG.Vector v a) => Array Vertex [Vertex] -> Vertex -> (m -> a -> a) -> (Vertex -> a) -> (a -> m) -> v a
scanTreeVG !tree !root !sact !acc0At !toM = VG.create $ do
  !dp <- VGM.unsafeNew nVerts

  !_ <- flip fix (-1, root) $ \runTreeDp (!parent, !v1) -> do
    let !v2s = filter (/= parent) $! tree ! v1
    !x1 <- foldM (\acc v2 -> (`sact` acc) . toM <$> runTreeDp (v1, v2)) (acc0At v1) v2s
    VGM.write dp v1 x1
    return x1

  return dp
  where
    !nVerts = rangeSize $! bounds tree

-- | Type-restricted `scanTreeVG`.
scanTreeVU :: (HasCallStack, U.Unbox a) => Array Vertex [Vertex] -> Vertex -> (m -> a -> a) -> (Vertex -> a) -> (a -> m) -> U.Vector a
scanTreeVU = scanTreeVG

-- | Type-restricted `scanTreeVG`.
scanTreeV :: HasCallStack => Array Vertex [Vertex] -> Vertex -> (m -> a -> a) -> (Vertex -> a) -> (a -> m) -> V.Vector a
scanTreeV = scanTreeVG

-- | \(O(N)\). Folds a tree for every vertex as a root using the rerooting technique.
-- REMARK: `mempty` is used for initial operator value.
--
-- = Typical problems
-- - [Typical 039 - Tree Distance (★5)](https://atcoder.jp/contests/typical90/tasks/typical90_am)
foldTreeAll :: (HasCallStack, U.Unbox a, U.Unbox m, MonoidAction m a) => Array Vertex [Vertex] -> (Vertex -> a) -> (a -> m) -> U.Vector a
foldTreeAll !tree !acc0At !toM =
  -- Calculate tree DP for one root vertex
  let !treeDp = scanTreeVG tree root0 mact acc0At toM
      !rootDp = U.create $ do
        -- Calculate tree DP for every vertex as a root:
        !dp <- UM.unsafeNew nVerts
        flip fix (-1, op0, root0) $ \runRootDp (!parent, !parentOp, !v1) -> do
          let !children = U.fromList . filter (/= parent) $! tree ! v1
          let !opL = U.scanl' (\op v2 -> (op <>) . toM $! treeDp U.! v2) op0 children
          let !opR = U.scanr' (\v2 op -> (<> op) . toM $! treeDp U.! v2) op0 children

          -- save
          let !x1 = (parentOp <> U.last opL) `mact` acc0At v1
          UM.write dp v1 x1

          flip U.imapM_ children $ \ !i2 !v2 -> do
            let !lrOp = (opL U.! i2) <> (opR U.! succ i2)
            let !v1Acc = (parentOp <> lrOp) `mact` acc0At v2
            runRootDp (v1, toM v1Acc, v2)

        return dp
   in rootDp
  where
    !nVerts = rangeSize $ bounds tree
    !root0 = 0 :: Int
    !op0 = mempty

-- }}}
