{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Tree.Lca where

import Algorithm.BinarySearch
import Control.Monad
import Control.Monad.Fix
import Control.Monad.ST
import Data.Array.IArray
import Data.BinaryLifting
import Data.Bits
import Data.Graph (Vertex)
import Data.List (find)
import Data.Maybe
import Data.SemigroupAction
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import ToyLib.Macro
import ToyLib.Prelude

-- {{{ LCA (basic)

-- | Vector for retrieving the parent vertex.
newtype ToParent = ToParent (VU.Vector Vertex)

instance Semigroup ToParent where
  (ToParent !vec1) <> (ToParent !vec2) = ToParent $ VU.map f vec2
    where
      !_ = dbgAssert (VG.length vec1 == VG.length vec2)
      f (-1) = (-1)
      f i = vec1 VU.! i

instance SemigroupAction ToParent Vertex where
  sact (ToParent !vec) !i = vec VU.! i

-- `(parents, depths, parents')`
type LcaCache = (ToParent, VU.Vector Int, BinaryLifting V.Vector ToParent)

-- | Returns `(parents, depths)` who maps vertices to the corresponding information.
-- REMARK: Use 0-based index for the graph vertices.
-- TODO: Consider using `Maybe Int` instead for easier `Monoid` integration
treeDepthInfo :: Int -> (Int -> [Int]) -> Int -> (ToParent, VU.Vector Int)
treeDepthInfo !nVerts !graph !root = runST $ do
  !parents <- VUM.replicate nVerts (-1 :: Int)
  !depths <- VUM.replicate nVerts (-1 :: Int)

  flip fix (0 :: Int, -1 :: Int, [root]) $ \loop (!depth, !parent, !vs) -> do
    forM_ vs $ \v -> do
      VUM.unsafeWrite depths v depth
      VUM.unsafeWrite parents v parent
      let !vs' = filter (/= parent) $ graph v
      loop (succ depth, v, vs')

  (,) <$> (ToParent <$> VU.unsafeFreeze parents) <*> VU.unsafeFreeze depths

-- | Returns `LcaCache`, i.e., `(parents, depths, parents')`.
lcaCache :: Int -> (Vertex -> [Vertex]) -> Vertex -> LcaCache
lcaCache !nVerts !graph !root = (toParent, depths, toParentN)
  where
    (!toParent, !depths) = treeDepthInfo nVerts graph root
    !toParentN = newBinLift toParent

-- | Returns the lowest common ancestor `(v, d)` with the help of the binary lifting technique.
-- REMARK: Use 0-based index for the graph vertices.
lca :: LcaCache -> Int -> Int -> (Int, Int)
lca (!_, !depths, !toParentN) !v1 !v2 = (vLCA, depths VU.! vLCA)
  where
    -- depths
    !d1 = depths VU.! v1
    !d2 = depths VU.! v2

    parentN = sactBL toParentN

    -- v1' and v2' are of the same depth:
    !v1' = if d1 <= d2 then v1 else v2
    !v2' = parentN (if d1 > d2 then v1 else v2) (abs $ d1 - d2)

    -- find the depth of the lowest common ancestor:
    !dLCA = fromJust . snd $ bsearch (0, min d1 d2) $ \d ->
      parentN v1' d /= parentN v2' d

    !vLCA = parentN v1' dLCA

-- | Gets the length between given two vertices with the help of LCA.
lcaLen :: LcaCache -> Int -> Int -> Int
lcaLen cache@(!_, !depths, !_) !v1 !v2 =
  let (!_, !d) = lca cache v1 v2
      !d1 = depths VU.! v1
      !d2 = depths VU.! v2
   in (d1 - d) + (d2 - d)

-- }}}

-- {{{ Tree path folding

-- | `ToParent` with monoid concatanation.
newtype ToParentM m = ToParentM (Int, VU.Vector m)

-- instance Semigroup

-- REMARK: My implementation is too slow.
-- FIXME: Stop the manual folding / binary lifting. Idea: monad?

-- | `LcaCache` with monoid folding on path.
type FoldLcaCache m = (LcaCache, V.Vector (VU.Vector m))

-- | Returns `FoldLcaCache` that can be used for calculating the folding value of path between two
-- vertices.
--
-- - graph: Vertex -> [Vertex]
-- - edgeValueOf: child -> parent -> m
foldLcaCache :: forall m. (Monoid m, VU.Unbox m) => Int -> (Vertex -> [Vertex]) -> Vertex -> (Vertex -> Vertex -> m) -> FoldLcaCache m
foldLcaCache !nVerts !graph !root !edgeValueOf = (cache, foldCache)
  where
    !cache@(!parents, !_, BinaryLifting !parents') = lcaCache nVerts graph root
    foldCache :: V.Vector (VU.Vector m)
    !foldCache = V.map snd $ newDoubling toParent appendArray
      where
        -- Monoid value when going up one parent vertex:
        !toParent = (0, VU.map f (rangeVG 0 (pred nVerts)))
          where
            f v = case parents `sact` v of
              (-1) -> mempty
              p -> edgeValueOf v p

        -- Folding function for the binary lifting technique:
        appendArray (!iBit, !ops) = (succ iBit, VU.imap f ops)
          where
            f !v0 !op =
              case ((parents' V.! iBit) `sact` v0) of
                (-1) -> op
                p -> op <> (ops VU.! p)

-- | `foldLcaCache` specific for `Array Vertex [(Vertex, a)]`.
foldLcaCache2 :: forall a m. (Monoid m, VU.Unbox m) => Array Int [(Vertex, a)] -> (a -> m) -> FoldLcaCache m
foldLcaCache2 !tree !toMonoid = foldLcaCache nVerts adj root getValue
  where
    !root = 0 :: Vertex
    !nVerts = rangeSize $ bounds tree
    adj = map fst . (tree !)
    -- FIXME: This is too slow.
    -- TODO: Do not iterate E^2 times.
    getValue v p = toMonoid . snd . fromJust . find ((== p) . fst) $ tree ! v

-- | Calculates the folding value of the path between two vertices in a tree.
foldViaLca :: forall m. (Monoid m, VU.Unbox m) => FoldLcaCache m -> Int -> Int -> m
foldViaLca (!cache@(!_, !depths, BinaryLifting !parents'), !ops') !v1 !v2 =
  let (!_, !d) = lca cache v1 v2
      -- !_ = dbg ((v1, d1), (v2, d2), (v, d), a1, a2, a1 <> a2)
      !d1 = depths VU.! v1
      !d2 = depths VU.! v2
      !a1 = foldParentN v1 (d1 - d)
      !a2 = foldParentN v2 (d2 - d)
   in a1 <> a2
  where
    -- Folds up the monoid value on going upwards:
    -- TODO: use `PermutationWithMonoid` to outsource the folding method
    foldParentN :: Vertex -> Int -> m
    foldParentN !v0 !nthParent = snd $ V.foldl' step (v0, mempty) input
      where
        !input = V.zip3 (rangeVG 0 62) parents' ops'
        step :: (Vertex, m) -> (Int, ToParent, VU.Vector m) -> (Vertex, m)
        step (!v, !acc) (!iBit, !parents, !ops)
          | testBit nthParent iBit = (parents `sact` v, acc <> (ops VU.! v))
          | otherwise = (v, acc)

-- }}}
