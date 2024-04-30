{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Heavy-light decomposition. Heavily inspired by @cojna/iota@.
--
-- = Edge path and vertex path
-- TODO: write about it.
module Data.Graph.Tree.Hld where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST
import Control.Monad.State.Class
import Control.Monad.Trans.State.Strict (execStateT)
import Data.Graph.Alias
import Data.Graph.Sparse
import Data.Maybe
import Data.Monoid (Dual (..))
import Data.SegmentTree.Strict
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import ToyLib.Debug

-- | Vertex re-ordered by HLD.
type VertexHLD = Vertex

-- | Heavy-light decomposition.
data HLD = HLD
  { -- | Vertex -> Parent vertex.
    parentHLD :: U.Vector Vertex,
    -- | Vertex -> Reorderd index.
    indexHLD :: U.Vector VertexHLD,
    -- | Vertex -> Their path's head vertex.
    pathHeadHLD :: U.Vector Vertex
  }
  deriving (Show, Eq)

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

        _ <- (\f -> fix f (-1) root) $ \loop p v1 -> do
          UM.write parent v1 p
          -- TODO: no need of vBig?
          (!size, (!eBig, !_vBig)) <- (\f -> U.foldM' f (1 :: Int, (-1, -1)) (tree `eAdj` v1)) $ \(!size, (!eBig, !vBig)) (!e2, !v2) -> do
            if v2 == p
              then return (size, (eBig, vBig))
              else do
                size2 <- loop v1 v2
                -- NOTE: It's `>` because we should swap at least once if there's some vertex other
                -- that the parent.
                return (size + size2, if size > size2 then (eBig, vBig) else (e2, v2))

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
  _ <- (`execStateT` (0 :: Int)) $ (\f -> fix f root (-1) root) $ \loop h p v1 -> do
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
    !_ = dbgAssert (2 * (nVertsSG tree - 1) == nEdgesSG tree) "hldOf: not a non-directed tree"
    !root = 0 :: Vertex

-- | \(O(log V)\).
--
-- = Typical Problems
-- [ABC 133 - F](https://atcoder.jp/contests/abc133/tasks/abc133_f)
lcaHLD :: HLD -> Vertex -> Vertex -> Vertex
lcaHLD HLD {..} = inner
  where
    -- inner x (-1) = 0 -- root
    inner !x !y
      -- sort for easier processing
      -- TODO: @case compare ix iy@ would be easier for me to understand
      | ix > iy = inner y x
      -- @x@ and @y@ are in other paths:
      | hx /= hy = inner x $ parentHLD U.! hy
      -- @x@ and @y@ are within the same path:
      -- select the smaller one, which is closer to the root and that is the LCA.
      | otherwise = x
      where
        !ix = indexHLD U.! x
        !iy = indexHLD U.! y
        hx = pathHeadHLD U.! x
        hy = pathHeadHLD U.! y

-- | Shared implementation of `edgePathHLD` and `vertPathHLD`.
_pathHLD :: Bool -> HLD -> Vertex -> Vertex -> [(VertexHLD, VertexHLD)]
_pathHLD isEdge HLD {..} x0 y0 = done $ inner x0 [] y0 []
  where
    done (!up, !down) = reverse up ++ down
    -- @up@: bottom to top. [(max, min)]
    -- @down@: top to bottom. [(min, max)]
    inner :: Vertex -> [(VertexHLD, VertexHLD)] -> Vertex -> [(VertexHLD, VertexHLD)] -> ([(VertexHLD, VertexHLD)], [(VertexHLD, VertexHLD)])
    inner x up y down
      | hx == hy && isEdge = case compare ix iy of
          LT -> (up, (ix {- edge -} + 1, iy) : down)
          GT -> ((ix, iy {- edge -} + 1) : up, down)
          EQ -> (up, down)
      | hx == hy && not isEdge = case compare ix iy of
          LT -> (up, (ix, iy) : down)
          _ -> ((ix, iy) : up, down)
      | otherwise = case compare ix iy of
          LT -> inner x up phy ((ihy, iy) : down)
          GT -> inner phx ((ix, ihx) : up) y down
          EQ -> error "unreachable"
      where
        ix, iy :: VertexHLD
        !ix = indexHLD U.! x
        !iy = indexHLD U.! y
        hx, hy :: Vertex
        hx = pathHeadHLD U.! x
        hy = pathHeadHLD U.! y
        ihx, ihy :: VertexHLD
        ihx = indexHLD U.! hx
        ihy = indexHLD U.! hy
        phx, phy :: VertexHLD
        phx = parentHLD U.! hx
        phy = parentHLD U.! hy

-- | \(O(log V)\) Returns inclusive edge vertex pairs.
-- - TODO: consider direction
edgePathHLD :: HLD -> Vertex -> Vertex -> [(VertexHLD, VertexHLD)]
edgePathHLD = _pathHLD True

-- | \(o(log V)\) Returns inclusive vertex pairs per HLD path.
-- - TODO: consider direction
vertPathHLD :: HLD -> Vertex -> Vertex -> [(VertexHLD, VertexHLD)]
vertPathHLD = _pathHLD False

_foldHLD :: (Monoid mono, Monad m) => Bool -> HLD -> (VertexHLD -> VertexHLD -> m mono) -> (VertexHLD -> VertexHLD -> m mono) -> Vertex -> Vertex -> m mono
_foldHLD isEdge hld f b v1 v2 = do
  (\g -> foldM g mempty (_pathHLD isEdge hld v1 v2)) $ \ !acc (!u, !v) -> do
    !x <-
      if u <= v
        then f u v
        else b v u
    return $! acc <> x

-- | Folds commutative monoid on tree edges using HLD.
--
-- = Segment tree
--
-- HLD path folding is often done with a segment tree. It uses `VertexHLD` as indices. If edges
-- have weights, you can either treat edges as new vertices or put weight to the depper index (when
-- (v1 /= v2) always holds.
--
-- Idea 1. Treat edges as new vertices. This is done with `foldVertsCommuteHLD`:
--
-- @
-- o--o--o  -> o-x-o-x-o
-- @
--
-- Idea 2. Put weight to deeper vertex. This is the idea of `foldEdgesCommuteHLD`:
--
-- @
--   o
--   | <--- edge 1
--   o <- write w1 here
--   | <--- edge 1
--   o <- write w2 here
-- @
--
-- = Typical Problems
-- - [ABC 294 - G](https://atcoder.jp/contests/abc294/tasks/abc294_g)
foldEdgesCommuteHLD :: (Monoid mono, Monad m) => HLD -> (VertexHLD -> VertexHLD -> m mono) -> Vertex -> Vertex -> m mono
foldEdgesCommuteHLD hld f = _foldHLD True hld f f

-- | TODO: verify
foldEdgesNonCommuteHLD :: (Monoid mono, Monad m) => HLD -> (VertexHLD -> VertexHLD -> m mono) -> (VertexHLD -> VertexHLD -> m mono) -> Vertex -> Vertex -> m mono
foldEdgesNonCommuteHLD hld f b = _foldHLD True hld f b

-- | Folds commutative monoid on tree vertices using HLD.
--
-- = Typical Problems
-- - [Vertex Add Path Sum - Library Checker](https://judge.yosupo.jp/problem/vertex_add_path_sum)
foldVertsCommuteHLD :: (Monoid mono, Monad m) => HLD -> (VertexHLD -> VertexHLD -> m mono) -> Vertex -> Vertex -> m mono
foldVertsCommuteHLD hld f = _foldHLD False hld f f

-- | Folds non-commutative monoid on tree vertices using HLD.
--
-- = Typical Problems
-- - [Vertex Set Path Composite - Library Checker](https://judge.yosupo.jp/problem/vertex_set_path_composite)
foldVertsNonCommuteHLD :: (Monoid mono, Monad m) => HLD -> (VertexHLD -> VertexHLD -> m mono) -> (VertexHLD -> VertexHLD -> m mono) -> Vertex -> Vertex -> m mono
foldVertsNonCommuteHLD = _foldHLD False

-- | API for folding a tree path of monoids with `HLD`.
data TreeMonoid a s = TreeMonoid
  { hldTM :: HLD,
    -- | Is it targetting commutative monoids?
    isCommuteTM :: Bool,
    -- | Is it targetting edge weights? (It's targetting vertex weights on no).
    isEdgeTM :: Bool,
    -- | Segment tree for folding upwards.
    streeFTM :: SegmentTree UM.MVector s a,
    -- | Segment tree in folding downwards. Only created when the monoid is not commutative.
    streeBTM :: SegmentTree UM.MVector s (Dual a)
  }

buildRawTM :: (PrimMonad m, Monoid a, U.Unbox a) => HLD -> Bool -> Bool -> U.Vector a -> m (TreeMonoid a (PrimState m))
buildRawTM hldTM isCommuteTM isEdgeTM xsRaw = do
  streeFTM <- buildSTree xsRaw
  streeBTM <-
    if isCommuteTM
      then buildSTree U.empty -- FIXME: is this safe?
      else buildSTree $ U.map Dual xsRaw
  return $ TreeMonoid {..}

-- | Builds a tree monoid on vertices.
buildVertTM :: (PrimMonad m, Monoid a, U.Unbox a) => HLD -> Bool -> U.Vector a -> m (TreeMonoid a (PrimState m))
buildVertTM hld@HLD {indexHLD} isCommuteTM xs_ = do
  let !xs = U.update (U.replicate (U.length xs_) mempty) $ U.imap (\i x -> (indexHLD U.! i, x)) xs_
  buildRawTM hld isCommuteTM False xs

-- | Map weightened edges into @(Vertex, a)@ pairs. The output is the input to `buildEdgeTM`.
edgeVertsHLD :: (U.Unbox a) => HLD -> U.Vector (Vertex, Vertex, a) -> U.Vector (Vertex, a)
edgeVertsHLD HLD {indexHLD} =
  U.map
    ( \(!u, !v, !w) ->
        -- REMARK: Return in `Vertex`, not in `VertexHLD` so that `writeTM` etc. work as expected.
        if indexHLD U.! u >= indexHLD U.! v
          then (u, w)
          else (v, w)
    )

-- | Builds a tree monoid on edges. **The input must be the output of `edgeVertsHLD`.**
buildEdgeTM :: (PrimMonad m, Monoid a, U.Unbox a) => HLD -> Bool -> U.Vector (Vertex, a) -> m (TreeMonoid a (PrimState m))
buildEdgeTM hld@HLD {indexHLD} isCommuteTM ixs = do
  let !n = U.length indexHLD
  let !xs = U.update (U.replicate n mempty) $ U.map (\(!v, !x) -> (indexHLD U.! v, x)) ixs
  buildRawTM hld isCommuteTM True xs

foldTM :: (PrimMonad m, Monoid a, U.Unbox a) => TreeMonoid a (PrimState m) -> Vertex -> Vertex -> m a
foldTM TreeMonoid {..} v1 v2
  | isCommuteTM = _foldHLD isEdgeTM hldTM (foldSTree streeFTM) (foldSTree streeFTM) v1 v2
  | otherwise = _foldHLD isEdgeTM hldTM (foldSTree streeFTM) ((fmap getDual .) . foldSTree streeBTM) v1 v2

readTM :: (PrimMonad m, U.Unbox a) => TreeMonoid a (PrimState m) -> Vertex -> m a
readTM TreeMonoid {..} i_ = do
  let !i = indexHLD hldTM U.! i_
  readSTree streeFTM i

writeTM :: (PrimMonad m, Monoid a, U.Unbox a) => TreeMonoid a (PrimState m) -> Vertex -> a -> m ()
writeTM TreeMonoid {..} i_ x = do
  let !i = indexHLD hldTM U.! i_
  writeSTree streeFTM i x
  -- TODO: resolve statically
  unless isCommuteTM $ do
    writeSTree streeBTM i $ Dual x

exchangeTM :: (PrimMonad m, Monoid a, U.Unbox a) => TreeMonoid a (PrimState m) -> Vertex -> a -> m a
exchangeTM TreeMonoid {..} i_ x = do
  let !i = indexHLD hldTM U.! i_
  !res <- exchangeSTree streeFTM i x
  -- TODO: resolve statically
  unless isCommuteTM $ do
    writeSTree streeBTM i $ Dual x
  return res

modifyTM :: (PrimMonad m, Monoid a, U.Unbox a) => TreeMonoid a (PrimState m) -> (a -> a) -> Int -> m ()
modifyTM TreeMonoid {..} f i_ = do
  let !i = indexHLD hldTM U.! i_
  modifySTree streeFTM f i
  -- TODO: resolve statically
  unless isCommuteTM $ do
    modifySTree streeBTM (Dual . f . getDual) i
