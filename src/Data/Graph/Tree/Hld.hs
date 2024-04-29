{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Heavy-light decomposition. Heavily inspired by @cojna/iota@.
module Data.Graph.Tree.Hld where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.ST
import Control.Monad.State.Class
import Control.Monad.Trans.State.Strict (State, StateT, evalState, evalStateT, execState, execStateT, runState, runStateT)
import Data.Graph.Alias
import Data.Graph.Sparse
import Data.Maybe
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

type PathIndexHLD = Int

-- | Heavy-light decomposition.
data HLD = HLD
  { -- | Vertex -> Parent vertex.
    parentHLD :: U.Vector Vertex,
    -- | Vertex -> Reorderd index.
    orderHLD :: U.Vector PathIndexHLD,
    -- | Vertex -> Their path's head vertex.
    pathHeadHLD :: U.Vector Vertex
  }
  deriving (Show, Eq)

-- | \(O(log V)\).
lcaHLD :: HLD -> Vertex -> Vertex -> Vertex
lcaHLD HLD {..} v1 v2 = inner v1 v2
  where
    inner x (-1) = 0 -- root
    inner !x !y
      -- sort for easier processing
      -- TODO: @case compare ix iy@ would be easier for me to understand
      | ix > iy = inner y x
      -- @x@ and @y@ are in other paths:
      -- TODO: why is it hx /= hy? isn't it ix /= iy? <-- because it's not path index?
      | hx /= hy = inner x $ parentHLD U.! hy
      -- @x@ and @y@ are within the same path:
      -- select the smaller one, which is closer to the root and that is the LCA.
      | otherwise = x
      where
        !ix = orderHLD U.! x
        !iy = orderHLD U.! y
        hx = pathHeadHLD U.! x
        hy = pathHeadHLD U.! y

-- | \(O(log V)\)
pathHLD :: HLD -> Vertex -> Vertex -> [(PathIndexHLD, PathIndexHLD)]
pathHLD HLD {..} = inner
  where
    -- TODO: when `y` is `-1`?
    inner !x !y
      -- sort for easier processing
      | ix > iy = inner y x
      | hx /= hy =
          let !ihy = orderHLD U.! hy
              !iy' = iy + 1
           in (ihy, iy') : inner x (parentHLD U.! hy)
      | ix == iy = []
      | otherwise = [(ix + 1, iy + 1)]
      where
        !ix = orderHLD U.! x
        !iy = orderHLD U.! y
        hx = pathHeadHLD U.! x
        hy = pathHeadHLD U.! y

-- | Heavy-light decomposition or Centroid Path Decomposition.
--
-- = About
-- HLD builds a smaller tree on top of an existing tree, combining vertices into paths.
--
-- = References
-- - https://take44444.github.io/Algorithm-Book/graph/tree/hld/main.html
hldOf :: forall w. SparseGraph Int w -> HLD
hldOf tree = runST $ do
  -- Re-create adjacent vertices so that the biggest subtree's head vertex comes first.
  --
  -- We /could/ instead record the biggest adjacent subtree vertex for each vertex, but the other
  -- DFS would be harder.
  let (!tree', !parent) = runST $ do
        adjVec <- U.thaw (adjacentsSG tree)
        parent <- UM.unsafeNew n

        (\f -> fix f (-1) root) $ \loop p v1 -> do
          UM.write parent v1 p
          (!size, (!eBig, !vBig)) <- (\f -> U.foldM' f (1, (-1, -1)) (tree `eAdj` v1)) $ \(!size, (!eBig, !vBig)) (!e2, !v2) -> do
            if v2 == p
              then return (size, (eBig, vBig))
              else do
                size2 <- loop v1 v2
                return (size + size2, if size >= size2 then (eBig, vBig) else (e2, v2))

          -- move the biggest subtree's head to the first adjacent vertex
          when (eBig /= -1) $ do
            UM.swap adjVec eBig $ fst (U.head (tree `eAdj` v1))
          return size

        !vec <- U.unsafeFreeze adjVec
        (tree {adjacentsSG = vec},) <$> U.unsafeFreeze parent

  -- vertex -> reordered vertex index
  order <- UM.replicate n (-1 :: Int)

  -- vertex -> head vertex of the path
  pathHead <- UM.replicate n (-1 :: Int)

  -- reorderd vertex index is stored as the state
  (`runStateT` (0 :: Int)) $ (\f -> fix f root (-1) root) $ \loop h p v1 -> do
    UM.write order v1 =<< get
    modify' (+ 1)

    UM.write pathHead v1 h
    let (!adj1, !rest) = fromJust $ U.uncons (tree' `adj` v1)

    -- when the first vertex is within the same path:
    when (adj1 /= p) $ do
      loop h v1 adj1

    -- the others are in other paths:
    U.forM_ rest $ \v2 -> do
      when (v2 /= p) $ do
        loop v2 v1 v2

  HLD parent <$> U.unsafeFreeze order <*> U.unsafeFreeze pathHead
  where
    n = nVertsSG tree
    !root = 0 :: Vertex
