module Tests.SplaySeq where

import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.Trans.State.Strict
import Data.Semigroup
import Data.SplaySeq
import Data.Vector.Unboxed qualified as U
import Debug.Trace
import Test.Tasty
import Test.Tasty.QuickCheck as QC

-- | Reads from left to right and right to left.
readLR :: (PrimMonad m) => SplaySeq (PrimState m) (Sum Int) -> StateT SplayIndex m (U.Vector (Sum Int))
readLR seq = do
  let !n = capacitySS seq
  !forwards <- U.generateM n $ \k -> do
    root' <- get
    let !_ = traceShow (n, (k, root')) ()
    (!root'', !x) <- readSS seq root' k
    put root''
    return x
  !backwards <- U.generateM n $ \k -> do
    root'' <- get
    let !_ = traceShow (n, (k, root'')) ()
    (!root''', !x) <- readSS seq root'' (n - 1 - k)
    put root'''
    return x
  return $ forwards U.++ backwards

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
              evalStateT (readLR seq) root
        return . QC.counterexample (show xs) $ (xs U.++ U.reverse xs) QC.=== U.map getSum res,
      QC.testProperty "SplaySeq: write" $ do
        n <- QC.chooseInt (1, maxN)
        xs <- U.fromList <$> QC.vectorOf n (QC.chooseInt rng)
        ys <- U.fromList <$> QC.vectorOf n (QC.chooseInt rng)
        let res = runST $ do
              seq <- newSS @(Sum Int) n
              root <- allocSeqSS seq $ U.map Sum xs
              root' <- (`execStateT` root) $ do
                U.iforM_ ys $ \i x -> do
                  root' <- get
                  root'' <- writeSS seq root' i $ Sum x
                  put root''
              evalStateT (readLR seq) root'
        return . QC.counterexample (show ys) $ (ys U.++ U.reverse ys) QC.=== U.map getSum res
    ]
  where
    maxN = 16
    rng = (-20, 20)

tests :: [TestTree]
tests = [testGroup "Data.SplaySeq" [randomTests]]
