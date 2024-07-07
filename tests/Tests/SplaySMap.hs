module Tests.SplaySMap where

import Control.Monad.ST
import Data.Ord
import Data.SplayMap
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Unboxed qualified as U
import Test.Tasty
import Test.Tasty.QuickCheck as QC

splayMapProps :: TestTree
splayMapProps =
  testGroup
    "Splay map properties"
    [ QC.testProperty "splay map: insert" $ do
        n <- QC.chooseInt (1, 100)
        ks <- U.fromList <$> QC.vectorOf n (QC.chooseInt (-100, 100))
        vs <- U.fromList <$> QC.vectorOf n (QC.chooseInt (-100, 100))
        let expected = U.modify (VAI.sortBy (comparing fst)) $ U.zip ks vs
        let xs' = runST $ do
              smap <- buildSMap n $ U.zip ks vs
              dfsSMap smap
        return $ xs' QC.=== expected
    ]

tests :: [TestTree]
tests = [testGroup "Data.SplayMap" [splayMapProps]]
