module Tests.SplayMap where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST
import Data.IntMap qualified as IM
import Data.SplayMap
import Data.Vector.Unboxed qualified as U
import Test.Tasty
import Test.Tasty.QuickCheck as QC

queryGen :: Gen (Int, Int, Int)
queryGen = do
  t <- QC.chooseInt (0, 1)
  k <- QC.chooseInt (0, 1)
  v <- QC.chooseInt (0, 1)
  return (t, k, v)

foldQueryIM :: IM.IntMap Int -> (Int, Int, Int) -> IM.IntMap Int
foldQueryIM !im (0, !k, !v) = IM.insert k v im
foldQueryIM !im (1, !k, !_) = IM.delete k im
foldQueryIM _ _ = error "unreachable"

processQuerySM :: (PrimMonad m) => SplayMap Int Int (PrimState m) -> (Int, Int, Int) -> m ()
processQuerySM sm (0, !k, !v) = do _ <- insertSM sm k v; return ()
processQuerySM sm (1, !k, !_) = do _ <- deleteSM sm k; return ()
processQuerySM _ _ = error "unreachable"

splayMapProps :: TestTree
splayMapProps =
  testGroup
    "Splay map properties"
    [ QC.testProperty "splay map: insert" $ do
        n <- QC.chooseInt (1, maxN)
        ks <- U.fromList <$> QC.vectorOf n (QC.chooseInt rng)
        vs <- U.fromList <$> QC.vectorOf n (QC.chooseInt rng)
        let expected = U.fromList . IM.assocs . IM.fromList . U.toList $ U.zip ks vs
        let xs' = runST $ do
              smap <- buildSM n $ U.zip ks vs
              dfsSM smap
        return . QC.counterexample (show (U.zip ks vs)) $ xs' QC.=== expected,
      QC.testProperty "splay map: insert/delete" $ do
        n <- QC.chooseInt (1, maxN)
        qs <- U.fromList <$> QC.vectorOf n queryGen
        let expected = U.fromList . IM.assocs $ U.foldl' foldQueryIM IM.empty qs
        let xs' = runST $ do
              sm <- newSM n
              U.forM_ qs $ processQuerySM sm
              dfsSM sm
        return . QC.counterexample (show qs) $ xs' QC.=== expected
    ]
  where
    maxN = 100
    rng = (-20, 20)

tests :: [TestTree]
tests = [testGroup "Data.SplayMap" [splayMapProps]]
