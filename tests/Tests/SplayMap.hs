module Tests.SplayMap where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST
import Data.Bifunctor (bimap)
import Data.IntMap qualified as IM
import Data.SplayMap
import Data.Vector.Unboxed qualified as U
import Test.Tasty
import Test.Tasty.QuickCheck as QC

queryGen :: (Int, Int) -> Gen (Int, Int, Int)
queryGen rng = do
  -- 0: insert, 1: delete, 2: lookupLT, 3: lookupLE, 4: lookupGT, 5: lookupGE
  t <- QC.chooseInt (0, 5)
  k <- QC.chooseInt rng
  v <- QC.chooseInt rng
  return (t, k, v)

foldQueryIM :: ([Maybe (Int, Int)], IM.IntMap Int) -> (Int, Int, Int) -> ([Maybe (Int, Int)], IM.IntMap Int)
-- insert/delete
foldQueryIM (!lookups, !im) (0, !k, !v) = (lookups, IM.insert k v im)
foldQueryIM (!lookups, !im) (1, !k, !_) = (lookups, IM.delete k im)
-- lookups
foldQueryIM (!lookups, !im) (2, !k, !_) = (IM.lookupLT k im : lookups, im)
foldQueryIM (!lookups, !im) (3, !k, !_) = (IM.lookupLE k im : lookups, im)
foldQueryIM (!lookups, !im) (4, !k, !_) = (IM.lookupGT k im : lookups, im)
foldQueryIM (!lookups, !im) (5, !k, !_) = (IM.lookupGE k im : lookups, im)
foldQueryIM _ _ = error "unreachable"

processQuerySM :: (PrimMonad m) => SplayMap Int Int (PrimState m) -> [Maybe (Int, Int)] -> (Int, Int, Int) -> m [Maybe (Int, Int)]
processQuerySM sm lookups (0, !k, !v) = do _ <- insertSM sm k v; return lookups
processQuerySM sm lookups (1, !k, !_) = do _ <- deleteSM sm k; return lookups
processQuerySM sm lookups (2, !k, !_) = do kv <- lookupLTSM sm k; return (kv : lookups)
processQuerySM sm lookups (3, !k, !_) = do kv <- lookupLESM sm k; return (kv : lookups)
processQuerySM sm lookups (4, !k, !_) = do kv <- lookupGTSM sm k; return (kv : lookups)
processQuerySM sm lookups (5, !k, !_) = do kv <- lookupGESM sm k; return (kv : lookups)
processQuerySM _ _ _ = error "unreachable"

intMapLikes :: TestTree
intMapLikes =
  testGroup
    "Splay map IntMap-like API tests"
    [ QC.testProperty "splay map: insert" $ do
        n <- QC.chooseInt (1, maxN)
        ks <- U.fromList <$> QC.vectorOf n (QC.chooseInt rng)
        vs <- U.fromList <$> QC.vectorOf n (QC.chooseInt rng)
        let expected = U.fromList . IM.assocs . IM.fromList . U.toList $ U.zip ks vs
        let xs' = runST $ do
              smap <- buildSM n $ U.zip ks vs
              dfsSM smap
        return . QC.counterexample (show (U.zip ks vs)) $ xs' QC.=== expected,
      QC.testProperty "splay map: insert/delete/lookup" $ do
        n <- QC.chooseInt (1, maxN)
        qs <- U.fromList <$> QC.vectorOf n (queryGen rng)
        let expected@(!_, !_) = bimap reverse (U.fromList . IM.assocs) $ U.foldl' foldQueryIM ([], IM.empty) qs
        let result@(!_, !_) = runST $ do
              sm <- newSM n
              lookups <- U.foldM' (processQuerySM sm) [] qs
              (reverse lookups,) <$> dfsSM sm
        return . QC.counterexample (show qs) $ result QC.=== expected
    ]
  where
    maxN = 500
    rng = (-20, 20)

tests :: [TestTree]
tests = [testGroup "Data.SplayMap" [intMapLikes]]
