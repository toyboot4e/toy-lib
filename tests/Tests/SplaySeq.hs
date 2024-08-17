module Tests.SplaySeq where

import Control.Monad.ST
import Control.Monad.Trans.State.Strict
import Data.Buffer
import Data.Semigroup
import Data.SplaySeq
import Data.Vector.Unboxed qualified as U
import Debug.Trace
import Test.Tasty
import Test.Tasty.QuickCheck as QC

randomTests :: TestTree
randomTests =
  testGroup
    "SplaySeq random tests"
    [ QC.testProperty "SplaySeq: insert" $ do
        n <- QC.chooseInt (1, maxN)
        xs <- U.fromList <$> QC.vectorOf n (QC.chooseInt rng)
        let res = runST $ do
              seq <- newSS @(Sum Int) n
              root <- allocSeqSS seq $ U.map Sum xs
              (!forwards, !root') <- (`runStateT` root) $ U.generateM n $ \k -> do
                root' <- get
                let !_ = traceShow (n, (k, root')) ()
                (!root'', !x) <- readSS seq root' k
                put root''
                return x
              (!backwards, !_) <- (`runStateT` root') $ U.generateM n $ \k -> do
                root'' <- get
                let !_ = traceShow (n, (k, root'')) ()
                (!root''', !x) <- readSS seq root'' (n - 1 - k)
                put root'''
                return x
              return $ forwards U.++ backwards
        return . QC.counterexample (show xs) $ (xs U.++ U.reverse xs) QC.=== U.map getSum res
    ]
  where
    maxN = 16
    rng = (-20, 20)

tests :: [TestTree]
tests = [testGroup "Data.SplaySeq" [randomTests]]
