module Tests.SplaySeq where

import Control.Monad.ST
import Control.Monad.Trans.State.Strict (evalStateT, get, put)
import Data.Buffer
import Data.Semigroup
import Data.SplaySeq
import Data.Vector.Unboxed qualified as U
import Test.Tasty
import Test.Tasty.QuickCheck as QC

randomTests :: TestTree
randomTests =
  testGroup
    "SplaySeq random tests"
    [ QC.testProperty "splay map: insert" $ do
        n <- QC.chooseInt (1, maxN)
        xs <- U.fromList <$> QC.vectorOf n (QC.chooseInt rng)
        let res = runST $ do
              seq <- newSS n
              root <- allocSeqSS seq $ U.map Sum xs
              (`evalStateT` root) $ U.generateM n $ \k -> do
                root' <- get
                (!root'', !x) <- readSS seq root' k
                put root''
                return x
        return . QC.counterexample (show xs) $ xs QC.=== (U.map getSum res)
    ]
  where
    maxN = 500
    rng = (-20, 20)

tests :: [TestTree]
tests = [testGroup "Data.SplaySeq" [randomTests]]
