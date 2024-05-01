{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Heavy-light decomposition or Centroid Path Decomposition construction. Heavily inspired by
-- @cojna/iota@ and @maspypy/library@.
--
-- HLD lets you find LCA in \(O(\log V)\) time and lets you fold monoids on a path in
-- \(O(\log^2 V)\) time with the help of segment trees.
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

-- | Vertex reindexed by `indexHLD`.
type VertexHLD = Vertex

-- | HLD splits a tree into lines. Lines are often called "paths", but here we say "lines" for
-- avoiding word conflicts.
--
-- = Example
--
-- == Original tree
--
-- Consider a tree with arbitrary vertex order:
--
-- @
--  0--8--7--3--1--2--12--13--15--14     XX: vertex
--     |        |                         --: edge
-- 10--5        11--8--6                   |: edge
--     |
--     4
-- @
--
-- == @indexHLD: Vertex -> vertexHLD@
--
-- The tree vertices are reindexed with `indexHLD`, ensuring vertices in each line have consecutive
-- numbers:
--
-- @
--  0==1==2==3==4==5==6==7==8==9     XX: vertex
--     |        |                     ==: edge on the same line
-- 14==13       10==11==12            |: edge between different lines
--     |
--     15
-- @
--
-- Note that vertices on higher lines are assigned smaller numbers.
--
-- == `headHLD: Vertex -> Vertex`
--
-- `headHLD` can be used for finding LCA of two vertices. To find the LCA, move up to the head, go
-- up to the parental line's vertex and repeat until they are on the same line.
--
-- @
--  0==0==0==0==0==0==0==0==0==0
--     |     |
--  5==5      11==11==11
--     |
--     4
-- @
--
-- `headHLD` also works for identifying lines. When two vertices are on the same line, they have the
-- same head.
--
-- == `parentHLD: Vertex -> Vertex`
--
-- `parentHLD` lets you go up to the parental line's vertex from a head:
--
-- @
-- (-1)==0==8==7==3==1==2==12==13==15
--       |        |
--    5==8        1==11=8
--       |
--       5
-- @
--
-- = Segment tree integration
--
-- With HLD, we can find a path between two vertices @u@, @v@: @u@ -> @lca(u, v)@ -> @v@. The path
-- is composed of line slices, which can be fastly folded with a segment tree.
--
-- == Vertex monoid folding on a path
--
-- If vertices have weights, use `foldVertsCommuteHLD` or `foldVertsHLD`.
--
-- == Edge monoid folding on a path
--
-- If edges have weights, you can either treat the edges as new vertices or assign edge weights to
-- the depper index.
--
-- Idea 1. Treat edges as new vertices.
--
-- @
--        convert
-- o--o--o  --> o-x-o-x-o
-- @
--
-- Idea 2. Assign edge weight to the deeper vertex. This is the idea of `foldEdgesCommuteHLD` and
-- `foldEdgesHLD`:
--
-- @
--   o
--   | <--- edge 1
--   o <- write wegiht 1 here
--   | <--- edge 2
--   o <- write wegiht 2 here
-- @
data HLD = HLD
  { -- | `Vertex` -> Parent `Vertex`.
    parentHLD :: !(U.Vector Vertex),
    -- | `Vertex` -> Reindexed vertex (`VertexHLD`).
    indexHLD :: !(U.Vector VertexHLD),
    -- | `Vertex` -> The line's head `Vertex`.
    headHLD :: !(U.Vector Vertex)
  }
  deriving (Show, Eq)

-- * Construction

-- | \(O(V)\) Constructs HLD.
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
          (!size, !eBig) <-
            U.foldM'
              ( \(!size, !eBig) (!e2, !v2) -> do
                  if v2 == p
                    then return (size, eBig)
                    else do
                      size2 <- loop v1 v2
                      -- NOTE: It's `>` because we should swap at least once if there's some vertex other
                      -- that the parent.
                      return (size + size2, if size > size2 then eBig else e2)
              )
              (1 :: Int, -1)
              (tree `eAdj` v1)
          -- move the biggest subtree's head to the first adjacent vertex
          when (eBig /= -1) $ do
            UM.swap adjVec eBig $ fst (U.head (tree `eAdj` v1))
          return size

        !vec <- U.unsafeFreeze adjVec
        (tree {adjacentsSG = vec},) <$> U.unsafeFreeze parent

  -- vertex -> reindexed vertex index
  indices <- UM.replicate n (-1 :: Int)

  -- vertex -> head vertex of the line
  heads <- UM.replicate n (-1 :: Int)

  -- reindexed vertex index is stored as the state
  _ <- (`execStateT` (0 :: Int)) $ (\f -> fix f root (-1) root) $ \loop h p v1 -> do
    UM.write indices v1 =<< get
    modify' (+ 1)

    UM.write heads v1 h
    let (!adj1, !rest) = fromJust $ U.uncons (tree' `adj` v1)

    -- when the first vertex is within the same line:
    when (adj1 /= p) $ do
      loop h v1 adj1

    -- the others are in other lines:
    U.forM_ rest $ \v2 -> do
      when (v2 /= p) $ do
        loop v2 v1 v2

  HLD parent <$> U.unsafeFreeze indices <*> U.unsafeFreeze heads
  where
    n = nVertsSG tree
    !_ = dbgAssert (2 * (nVertsSG tree - 1) == nEdgesSG tree) "hldOf: not a non-directed tree"
    !root = 0 :: Vertex

-- * LCA and paths

-- | \(O(\log V)\) HLD calculation.
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
      -- @x@ and @y@ are in other lines:
      | hx /= hy = inner x $ parentHLD U.! hy
      -- @x@ and @y@ are within the same line:
      -- select the smaller one, which is closer to the root and that is the LCA.
      | otherwise = x
      where
        !ix = indexHLD U.! x
        !iy = indexHLD U.! y
        hx = headHLD U.! x
        hy = headHLD U.! y

-- | \(O(\log V)\) Shared implementation of `edgePathHLD` and `vertPathHLD`.
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
        hx = headHLD U.! x
        hy = headHLD U.! y
        ihx, ihy :: VertexHLD
        ihx = indexHLD U.! hx
        ihy = indexHLD U.! hy
        phx, phy :: VertexHLD
        phx = parentHLD U.! hx
        phy = parentHLD U.! hy

-- | \(O(\log V)\) Returns inclusive edge vertex pairs.
edgePathHLD :: HLD -> Vertex -> Vertex -> [(VertexHLD, VertexHLD)]
edgePathHLD = _pathHLD True

-- | \(O(\log V)\) Returns inclusive vertex pairs per HLD line.
vertPathHLD :: HLD -> Vertex -> Vertex -> [(VertexHLD, VertexHLD)]
vertPathHLD = _pathHLD False

-- * Folding methods

-- | \(O(\log^2 V)\) The shared implementation of @fold*HLD@ variants.
_foldHLD :: (Monoid mono, Monad m) => Bool -> HLD -> (VertexHLD -> VertexHLD -> m mono) -> (VertexHLD -> VertexHLD -> m mono) -> Vertex -> Vertex -> m mono
_foldHLD isEdge hld f b v1 v2 = do
  foldM
    ( \ !acc (!u, !v) -> do
        !x <-
          if u <= v
            then f u v
            else b v u
        return $! acc <> x
    )
    mempty
    (_pathHLD isEdge hld v1 v2)

-- | \(O(\log^2 V)\) Folds commutative monoids on a tree edges using HLD. Prefer to use the wrapper
-- `TreeMonoid`.
--
-- == Typical Problems
-- - [ABC 294 - G](https://atcoder.jp/contests/abc294/tasks/abc294_g)
foldEdgesCommuteHLD :: (Monoid mono, Monad m) => HLD -> (VertexHLD -> VertexHLD -> m mono) -> Vertex -> Vertex -> m mono
foldEdgesCommuteHLD hld f = _foldHLD True hld f f

-- | \(O(\log^2 V)\) Folds commutative monoids on a tree vertices using HLD. Prefer to use the
-- wrapper `TreeMonoid`.
--
-- TODO: verify
foldEdgesHLD :: (Monoid mono, Monad m) => HLD -> (VertexHLD -> VertexHLD -> m mono) -> (VertexHLD -> VertexHLD -> m mono) -> Vertex -> Vertex -> m mono
foldEdgesHLD hld f b = _foldHLD True hld f b

-- | \(O(\log^2 V)\) Folds commutative monoids on a tree vertices using HLD. Prefer to use the
-- wrapper `TreeMonoid`.
--
-- == Typical Problems
-- - [Vertex Add Path Sum - Library Checker](https://judge.yosupo.jp/problem/vertex_add_path_sum)
foldVertsCommuteHLD :: (Monoid mono, Monad m) => HLD -> (VertexHLD -> VertexHLD -> m mono) -> Vertex -> Vertex -> m mono
foldVertsCommuteHLD hld f = _foldHLD False hld f f

-- | \(O(\log^2 V)\) Folds non-commutative monoids on a tree vertices using HLD. Prefer to use the
-- wrapper `TreeMonoid`.
--
-- == Typical Problems
-- - [Vertex Set Path Composite - Library Checker](https://judge.yosupo.jp/problem/vertex_set_path_composite)
foldVertsHLD :: (Monoid mono, Monad m) => HLD -> (VertexHLD -> VertexHLD -> m mono) -> (VertexHLD -> VertexHLD -> m mono) -> Vertex -> Vertex -> m mono
foldVertsHLD = _foldHLD False

-- * `TreeMonoid`

-- | HLD wrapper for folding paths on a tree using `HLD` and segment tree(s).
data TreeMonoid a s = TreeMonoid
  { -- | Borrowed HLD.
    hldTM :: !HLD,
    -- | Indicates if it's targetting commutative monoids.
    isCommuteTM :: !Bool,
    -- | Indicates if it's targetting edge weights (If not, it's targetting vertex weights).
    isEdgeTM :: !Bool,
    -- | Segment tree for folding upwards.
    streeFTM :: !(SegmentTree UM.MVector s a),
    -- | Segment tree for folding downwards. Only created when the monoid is not commutative.
    streeBTM :: !(SegmentTree UM.MVector s (Dual a))
  }

-- ** Construction

-- | \(O(V)\) Shared implementation of `buildVertTM` and `buildEdgeTM`.
_buildRawTM :: (PrimMonad m, Monoid a, U.Unbox a) => HLD -> Bool -> Bool -> U.Vector a -> m (TreeMonoid a (PrimState m))
_buildRawTM hldTM isCommuteTM isEdgeTM xsRaw = do
  streeFTM <- buildSTree xsRaw
  streeBTM <-
    if isCommuteTM
      then buildSTree U.empty
      else buildSTree $ U.map Dual xsRaw
  return $ TreeMonoid {..}

-- | \(O(V)\) Builds a `TreeMonoid` on vertices.
buildVertTM :: (PrimMonad m, Monoid a, U.Unbox a) => HLD -> Bool -> U.Vector a -> m (TreeMonoid a (PrimState m))
buildVertTM hld@HLD {indexHLD} isCommuteTM xs_ = do
  let !xs = U.update (U.replicate (U.length xs_) mempty) $ U.imap (\i x -> (indexHLD U.! i, x)) xs_
  _buildRawTM hld isCommuteTM False xs

-- | \(O(V)\) Map weightened edges into @(Vertex, a)@ pairs. The output is the input to `buildEdgeTM`.
edgeVertsHLD :: (U.Unbox a) => HLD -> U.Vector (Vertex, Vertex, a) -> U.Vector (Vertex, a)
edgeVertsHLD HLD {indexHLD} =
  U.map
    ( \(!u, !v, !w) ->
        -- REMARK: Return in `Vertex`, not in `VertexHLD` so that `writeTM` etc. work as expected.
        if indexHLD U.! u >= indexHLD U.! v
          then (u, w)
          else (v, w)
    )

-- | \(O(V)\) Builds a `TreeMonoid` on edges. **The input must be the output of `edgeVertsHLD`**.
buildEdgeTM :: (PrimMonad m, Monoid a, U.Unbox a) => HLD -> Bool -> U.Vector (Vertex, a) -> m (TreeMonoid a (PrimState m))
buildEdgeTM hld@HLD {indexHLD} isCommuteTM ixs = do
  let !n = U.length indexHLD
  let !xs = U.update (U.replicate n mempty) $ U.map (\(!v, !x) -> (indexHLD U.! v, x)) ixs
  _buildRawTM hld isCommuteTM True xs

-- ** Segment tree methods

-- | \(O(log^2 V)\) Folds `TreeMonoid` on a path between two vertices
foldTM :: (PrimMonad m, Monoid a, U.Unbox a) => TreeMonoid a (PrimState m) -> Vertex -> Vertex -> m a
foldTM TreeMonoid {..} v1 v2
  | isCommuteTM = _foldHLD isEdgeTM hldTM (foldSTree streeFTM) (foldSTree streeFTM) v1 v2
  | otherwise = _foldHLD isEdgeTM hldTM (foldSTree streeFTM) ((fmap getDual .) . foldSTree streeBTM) v1 v2

-- | \(O(log V)\) Reads a `TreeMonoid` value on a `Vertex`.
readTM :: (PrimMonad m, U.Unbox a) => TreeMonoid a (PrimState m) -> Vertex -> m a
readTM TreeMonoid {..} i_ = do
  let !i = indexHLD hldTM U.! i_
  readSTree streeFTM i

-- | \(O(log V)\) Write a `TreeMonoid` value on a `Vertex`.
writeTM :: (PrimMonad m, Monoid a, U.Unbox a) => TreeMonoid a (PrimState m) -> Vertex -> a -> m ()
writeTM TreeMonoid {..} i_ x = do
  let !i = indexHLD hldTM U.! i_
  writeSTree streeFTM i x
  -- TODO: resolve statically
  unless isCommuteTM $ do
    writeSTree streeBTM i $ Dual x

-- | \(O(log V)\) Exchanges a `TreeMonoid` value on a `Vertex`.
exchangeTM :: (PrimMonad m, Monoid a, U.Unbox a) => TreeMonoid a (PrimState m) -> Vertex -> a -> m a
exchangeTM TreeMonoid {..} i_ x = do
  let !i = indexHLD hldTM U.! i_
  !res <- exchangeSTree streeFTM i x
  -- TODO: resolve statically
  unless isCommuteTM $ do
    writeSTree streeBTM i $ Dual x
  return res

-- | \(O(log V)\) Modifies a `TreeMonoid` value on a `Vertex`.
modifyTM :: (PrimMonad m, Monoid a, U.Unbox a) => TreeMonoid a (PrimState m) -> (a -> a) -> Int -> m ()
modifyTM TreeMonoid {..} f i_ = do
  let !i = indexHLD hldTM U.! i_
  modifySTree streeFTM f i
  -- TODO: resolve statically
  unless isCommuteTM $ do
    modifySTree streeBTM (Dual . f . getDual) i
