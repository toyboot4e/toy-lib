module Tests.SplaySMap where

import Control.Monad.ST
import Data.IntMap qualified as IM
import Data.SplayMap
import Data.Vector.Unboxed qualified as U
import Test.Tasty
import Test.Tasty.QuickCheck as QC

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
              smap <- buildSMap n $ U.zip ks vs
              dfsSMap smap
        return . QC.counterexample (show (U.zip ks vs)) $ xs' QC.=== expected
    ]
  where
    maxN = 100
    rng = (-50, 50)

tests :: [TestTree]
tests = [testGroup "Data.SplayMap" [splayMapProps]]
