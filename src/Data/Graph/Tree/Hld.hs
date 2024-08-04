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
import Data.Ix (inRange)
import Data.Maybe
import Data.Monoid (Dual (..))
import Data.SegmentTree.Strict
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
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
-- is decomposed into segments where segment trees can be integrated as in `TreeMonoid`.
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
--
-- = Subtree folding
--
-- Subtree `VertexHLD` is contiguous. We can easily find the corredponding @(l, r)@ paris of
-- `VertexHLD` that corresopnds to the subtree by remembering the subtree sizes.
data HLD = HLD
  { -- | The root vertex.
    rootHLD :: {-# UNPACK #-} !Vertex,
    -- | `Vertex` -> Parent `Vertex`.
    parentHLD :: !(U.Vector Vertex),
    -- | `Vertex` -> `VertexHLD` (re-indexed vertex that is contiguous in each segment).
    indexHLD :: !(U.Vector VertexHLD),
    -- | `Vertex` -> The line's head `Vertex`.
    headHLD :: !(U.Vector Vertex),
    -- | `VertexHLD` -> `Vertex`. Used for `ancestorHLD` etc.
    revIndexHLD :: !(U.Vector Vertex),
    -- | Depth information for `jumpHLD` etc.
    depthHLD :: !(U.Vector Int),
    -- | `Vertex` -> subtree size. This is for subtree folding.
    subtreeSizeHLD :: !(U.Vector Int)
  }
  deriving (Show, Eq)

-- * Construction

-- | \(O(V)\) Constructs HLD.
hldOf :: forall w. SparseGraph w -> HLD
hldOf tree = hldOf' tree 0

-- | \(O(V)\) Constructs HLD with root vertex specified.
hldOf' :: forall w. SparseGraph w -> Vertex -> HLD
hldOf' tree root = runST $ do
  -- Re-create adjacent vertices so that the biggest subtree's head vertex comes first.
  --
  -- We /could/ instead record the biggest adjacent subtree vertex for each vertex, but the other
  -- DFS would be harder.
  let (!tree', !parent, !depths, !subtreeSize) = runST $ do
        adjVec <- U.thaw (adjacentsSG tree)
        parent <- UM.unsafeNew n
        depths <- UM.unsafeNew n
        subtreeSize <- UM.unsafeNew n

        _ <- (\f -> fix f 0 (-1) root) $ \loop depth p v1 -> do
          GM.write parent v1 p
          GM.write depths v1 depth

          (!size1, !eBig) <-
            U.foldM'
              ( \(!size1, !eBig) (!e2, !v2) -> do
                  if v2 == p
                    then return (size1, eBig)
                    else do
                      size2 <- loop (depth + 1) v1 v2
                      -- NOTE: It's `>` because we should swap at least once if there's some vertex other
                      -- that the parent.
                      return (size1 + size2, if size1 > size2 then eBig else e2)
              )
              (1 :: Int, -1)
              (tree `eAdj` v1)

          -- move the biggest subtree's head to the first adjacent vertex.
          -- it means the "heavy edge" or the longest segment.
          when (eBig /= -1) $ do
            GM.swap adjVec eBig $ fst (G.head (tree `eAdj` v1))

          -- record subtree size
          GM.write subtreeSize v1 size1

          return size1

        !vec <- U.unsafeFreeze adjVec
        (tree {adjacentsSG = vec},,,)
          <$> U.unsafeFreeze parent
          <*> U.unsafeFreeze depths
          <*> U.unsafeFreeze subtreeSize

  -- vertex -> reindexed vertex index
  indices <- UM.replicate n (-1 :: Int)

  -- vertex -> head vertex of the line
  heads <- UM.replicate n (-1 :: Int)

  _ <- (`execStateT` (0 :: Int)) $ (\f -> fix f root (-1) root) $ \loop h p v1 -> do
    -- reindex:
    GM.write indices v1 =<< get
    modify' (+ 1)

    GM.write heads v1 h

    -- when the first vertex is within the same line:
    let (!adj1, !rest) = fromJust $ U.uncons (tree' `adj` v1)
    when (adj1 /= p) $ do
      loop h v1 adj1

    -- the others are in other lines:
    U.forM_ rest $ \v2 -> do
      when (v2 /= p) $ do
        loop v2 v1 v2

  !indices' <- U.unsafeFreeze indices
  let !revIndex = U.update (U.replicate n (-1)) $ U.imap (flip (,)) indices'

  HLD root parent indices'
    <$> U.unsafeFreeze heads
    <*> return revIndex
    <*> return depths
    <*> return subtreeSize
  where
    !n = nVertsSG tree
    !_ = dbgAssert (2 * (nVertsSG tree - 1) == nEdgesSG tree) "hldOf: not a non-directed tree"

-- * LCA

-- | \(O(\log V)\) HLD calculation.
--
-- = Typical Problems
-- [ABC 133 - F](https://atcoder.jp/contests/abc133/tasks/abc133_f)
lcaHLD :: HLD -> Vertex -> Vertex -> Vertex
lcaHLD HLD {..} = inner
  where
    inner !x !y
      -- sort for easier processing
      -- TODO: @case compare ix iy@ would be easier for me to understand
      | ix > iy = inner y x
      -- @x@ and @y@ are in other lines:
      | hx /= hy = inner x $ parentHLD G.! hy
      -- @x@ and @y@ are within the same line:
      -- select the smaller one, which is closer to the root and that is the LCA.
      | otherwise = x
      where
        !ix = indexHLD G.! x
        !iy = indexHLD G.! y
        hx = headHLD G.! x
        hy = headHLD G.! y

-- * Segments and folding methods

-- | \(O(\log V)\) Shared implementation of `edgeSegmentsHLD` and `vertSegmentsHLD`, which returns `[l, r]`
-- pairs for each segment.
--
-- - The return type is `VertexHLD`.
-- - Note that each @(l, r)@ pair can be @l > r@.
-- - LCA is omitted when @isEdge@ parameter is set to @True@ (the trick to put edge weights to
--   vertices).
_segmentsHLD :: Bool -> HLD -> Vertex -> Vertex -> [(VertexHLD, VertexHLD)]
_segmentsHLD isEdge HLD {..} x0 y0 = done $ inner x0 [] y0 []
  where
    done (!up, !down) = reverse up ++ down
    -- @up@: bottom to top. [(max, min)]
    -- @down@: top to bottom. [(min, max)]
    inner :: Vertex -> [(VertexHLD, VertexHLD)] -> Vertex -> [(VertexHLD, VertexHLD)] -> ([(VertexHLD, VertexHLD)], [(VertexHLD, VertexHLD)])
    inner x up y down
      | hx == hy && isEdge = case compare ix iy of
          -- skip LCA on edge vertices
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
        !ix = indexHLD G.! x
        !iy = indexHLD U.! y
        hx, hy :: Vertex
        hx = headHLD G.! x
        hy = headHLD G.! y
        ihx, ihy :: VertexHLD
        ihx = indexHLD G.! hx
        ihy = indexHLD G.! hy
        phx, phy :: VertexHLD
        phx = parentHLD G.! hx
        phy = parentHLD G.! hy

-- | \(O(\log V)\) Returns inclusive edge vertex pairs.
edgeSegmentsHLD :: HLD -> Vertex -> Vertex -> [(VertexHLD, VertexHLD)]
edgeSegmentsHLD = _segmentsHLD True

-- | \(O(\log V)\) Returns inclusive vertex pairs per.
vertSegmentsHLD :: HLD -> Vertex -> Vertex -> [(VertexHLD, VertexHLD)]
vertSegmentsHLD = _segmentsHLD False

-- | \(O(\log^2 V)\) Folds path between @v1@ and @v2@. Use specialized methods for simplisity.
foldHLD :: (Monoid mono, Monad m) => Bool -> HLD -> (VertexHLD -> VertexHLD -> m mono) -> (VertexHLD -> VertexHLD -> m mono) -> Vertex -> Vertex -> m mono
foldHLD isEdge hld foldF foldB v1 v2 = do
  foldM
    ( \ !acc (!u, !v) -> do
        !x <-
          if u <= v
            then foldF u v
            else foldB v u
        return $! acc <> x
    )
    mempty
    (_segmentsHLD isEdge hld v1 v2)

-- | \(O(\log^2 V)\) Folds commutative monoids on a tree edges using HLD. Prefer to use the wrapper
-- `TreeMonoid`.
--
-- == Typical Problems
-- - [ABC 294 - G](https://atcoder.jp/contests/abc294/tasks/abc294_g)
foldEdgesCommuteHLD :: (Monoid mono, Monad m) => HLD -> (VertexHLD -> VertexHLD -> m mono) -> Vertex -> Vertex -> m mono
foldEdgesCommuteHLD hld f = foldHLD True hld f f

-- | \(O(\log^2 V)\) Folds commutative monoids on a tree vertices using HLD. Prefer to use the
-- wrapper `TreeMonoid`.
--
-- TODO: verify
foldEdgesHLD :: (Monoid mono, Monad m) => HLD -> (VertexHLD -> VertexHLD -> m mono) -> (VertexHLD -> VertexHLD -> m mono) -> Vertex -> Vertex -> m mono
foldEdgesHLD = foldHLD True

-- | \(O(\log^2 V)\) Folds commutative monoids on a tree vertices using HLD. Prefer to use the
-- wrapper `TreeMonoid`.
--
-- == Typical Problems
-- - [Vertex Add Path Sum - Library Checker](https://judge.yosupo.jp/problem/vertex_add_path_sum)
foldVertsCommuteHLD :: (Monoid mono, Monad m) => HLD -> (VertexHLD -> VertexHLD -> m mono) -> Vertex -> Vertex -> m mono
foldVertsCommuteHLD hld f = foldHLD False hld f f

-- | \(O(\log^2 V)\) Folds non-commutative monoids on a tree vertices using HLD. Prefer to use the
-- wrapper `TreeMonoid`.
--
-- == Typical Problems
-- - [Vertex Set Path Composite - Library Checker](https://judge.yosupo.jp/problem/vertex_set_path_composite)
foldVertsHLD :: (Monoid mono, Monad m) => HLD -> (VertexHLD -> VertexHLD -> m mono) -> (VertexHLD -> VertexHLD -> m mono) -> Vertex -> Vertex -> m mono
foldVertsHLD = foldHLD False

-- * Subtree

-- | \(O(1)\) Returns @(start, end)@ `VertexHLD` that corresponds to the subtree segments rooted at
-- the @subtreeRoot@.
subtreeSegmentsHLD :: HLD -> Vertex -> (VertexHLD, VertexHLD)
subtreeSegmentsHLD HLD {..} subtreeRoot = (ir, ir + sr - 1)
  where
    ir = indexHLD G.! subtreeRoot
    sr = subtreeSizeHLD G.! subtreeRoot

-- | \(O(1)\) Returns @True@ if @u@ is in a subtree of @r@.
isInSubtreeHLD :: HLD -> Vertex -> Vertex -> Bool
isInSubtreeHLD hld@HLD {..} r u = inRange (l, r) iu
  where
    (!l, !r) = subtreeSegmentsHLD hld r
    !iu = indexHLD G.! u

-- * Jump

-- | \(O(\log V)\) Go up @i@ times from the parent node to the implicit root node.
ancestorHLD :: HLD -> Vertex -> Vertex -> Vertex
ancestorHLD HLD {..} parent k0 = inner parent k0
  where
    !_ = dbgAssert (k0 <= depthHLD G.! parent)
    inner v k
      -- on this segment
      | k <= iv - ihv = revIndexHLD G.! (iv - k)
      -- next segment
      | otherwise = inner (parentHLD U.! hv) (k - (iv - ihv + 1))
      where
        iv = indexHLD G.! v
        hv = headHLD G.! v
        ihv = indexHLD G.! hv

-- TODO: levelAncestorHLD: https://37zigen.com/level-ancestor-problem/

-- | \(O(\log V)\) Returns i-th vertex of a path between @u@, @v@.
--
-- <https://judge.yosupo.jp/problem/jump_on_tree>
jumpHLD :: HLD -> Vertex -> Vertex -> Int -> Maybe Vertex
jumpHLD hld@HLD {..} u v i
  | i > lenU + lenV = Nothing
  | i <= lenU = Just $ ancestorHLD hld u i
  | otherwise = Just $ ancestorHLD hld v (lenU + lenV - i)
  where
    lca = lcaHLD hld u v
    du = depthHLD G.! u
    dv = depthHLD G.! v
    lenU = du - depthHLD G.! lca
    lenV = dv - depthHLD G.! lca

-- * Utilities

-- | \(O(\log V)\) Returns path between @u@ and @v@.
pathHLD :: HLD -> Vertex -> Vertex -> [Vertex]
pathHLD hld@HLD {..} u v = concatMap expand $ _segmentsHLD False hld u v
  where
    expand (!l, !r)
      | l <= r = map (revIndexHLD G.!) [l .. r]
      | otherwise = map (revIndexHLD G.!) [r, r - 1 .. l]

-- * `TreeMonoid` for path folding

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
  let !xs = U.update (U.replicate (U.length xs_) mempty) $ U.imap (\i x -> (indexHLD G.! i, x)) xs_
  _buildRawTM hld isCommuteTM False xs

-- | \(O(V)\) Map weightened edges into @(Vertex, a)@ pairs. The output is the input to `buildEdgeTM`.
edgeVertsHLD :: (U.Unbox a) => HLD -> U.Vector (Vertex, Vertex, a) -> U.Vector (Vertex, a)
edgeVertsHLD HLD {indexHLD} =
  U.map
    ( \(!u, !v, !w) ->
        -- REMARK: Return in `Vertex`, not in `VertexHLD` so that `writeTM` etc. work as expected.
        if indexHLD G.! u >= indexHLD G.! v
          then (u, w)
          else (v, w)
    )

-- | \(O(V)\) Builds a `TreeMonoid` on edges. **The input must be the output of `edgeVertsHLD`**.
buildEdgeTM :: (PrimMonad m, Monoid a, U.Unbox a) => HLD -> Bool -> U.Vector (Vertex, a) -> m (TreeMonoid a (PrimState m))
buildEdgeTM hld@HLD {indexHLD} isCommuteTM ixs = do
  let !n = U.length indexHLD
  let !xs = U.update (U.replicate n mempty) $ U.map (\(!v, !x) -> (indexHLD G.! v, x)) ixs
  _buildRawTM hld isCommuteTM True xs

-- ** Segment tree methods

-- | \(O(log^2 V)\) Folds a path between two vertices.
foldTM :: (PrimMonad m, Monoid a, U.Unbox a) => TreeMonoid a (PrimState m) -> Vertex -> Vertex -> m a
foldTM TreeMonoid {..} v1 v2
  | isCommuteTM = foldHLD isEdgeTM hldTM (foldSTree streeFTM) (foldSTree streeFTM) v1 v2
  | otherwise = foldHLD isEdgeTM hldTM (foldSTree streeFTM) ((fmap getDual .) . foldSTree streeBTM) v1 v2

-- | \(O(log V)\) Folds commute monoids on vertives of a subtree.
foldSubtreeVertsTM :: (PrimMonad m, Monoid a, U.Unbox a) => TreeMonoid a (PrimState m) -> Vertex -> m a
foldSubtreeVertsTM TreeMonoid {..} subtreeRoot = foldSTree streeFTM l r
  where
    (!l, !r) = subtreeSegmentsHLD hldTM subtreeRoot

-- | \(O(log V)\) Folds commute monoids on edges of a subtree. TODO: test
foldSubtreeEdgeTM :: (PrimMonad m, Monoid a, U.Unbox a) => TreeMonoid a (PrimState m) -> Vertex -> m a
foldSubtreeEdgeTM TreeMonoid {..} subtreeRoot
  | l == r = return mempty
  | otherwise = foldSTree streeFTM (l + 1) r
  where
    (!l, !r) = subtreeSegmentsHLD hldTM subtreeRoot

-- | \(O(log V)\) Reads a `TreeMonoid` value on a `Vertex`.
readTM :: (PrimMonad m, U.Unbox a) => TreeMonoid a (PrimState m) -> Vertex -> m a
readTM TreeMonoid {..} i_ = do
  let !i = indexHLD hldTM G.! i_
  readSTree streeFTM i

-- | \(O(log V)\) Write a `TreeMonoid` value on a `Vertex`.
writeTM :: (PrimMonad m, Monoid a, U.Unbox a) => TreeMonoid a (PrimState m) -> Vertex -> a -> m ()
writeTM TreeMonoid {..} i_ x = do
  let !i = indexHLD hldTM G.! i_
  writeSTree streeFTM i x
  -- TODO: resolve statically
  unless isCommuteTM $ do
    writeSTree streeBTM i $ Dual x

-- | \(O(log V)\) Exchanges a `TreeMonoid` value on a `Vertex`.
exchangeTM :: (PrimMonad m, Monoid a, U.Unbox a) => TreeMonoid a (PrimState m) -> Vertex -> a -> m a
exchangeTM TreeMonoid {..} i_ x = do
  let !i = indexHLD hldTM G.! i_
  !res <- exchangeSTree streeFTM i x
  -- TODO: resolve statically
  unless isCommuteTM $ do
    writeSTree streeBTM i $ Dual x
  return res

-- | \(O(log V)\) Modifies a `TreeMonoid` value on a `Vertex`.
modifyTM :: (PrimMonad m, Monoid a, U.Unbox a) => TreeMonoid a (PrimState m) -> (a -> a) -> Int -> m ()
modifyTM TreeMonoid {..} f i_ = do
  let !i = indexHLD hldTM G.! i_
  modifySTree streeFTM f i
  -- TODO: resolve statically
  unless isCommuteTM $ do
    modifySTree streeBTM (Dual . f . getDual) i
