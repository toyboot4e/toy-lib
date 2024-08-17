module Tests.SplaySeq where

import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.Trans.State.Strict
import Data.Semigroup
import Data.SplaySeq
import Data.Vector.Unboxed qualified as U
import Data.Vector.Generic qualified as G
import Debug.Trace
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Tests.Util

-- | Reads from left to right and right to left.
readLR :: (PrimMonad m) => SplaySeq (PrimState m) (Sum Int) -> StateT SplayIndex m (U.Vector (Sum Int))
readLR seq = do
  let !n = capacitySS seq
  !forwards <- U.generateM n $ \k -> do
    root' <- get
    (!root'', !x) <- readSS seq root' k
    put root''
    return x
  !backwards <- U.generateM n $ \k -> do
    root'' <- get
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
        return . QC.counterexample (show ys) $ U.map getSum res QC.=== (ys U.++ U.reverse ys),
      QC.testProperty "SplaySeq: fold" $ do
        n <- QC.chooseInt (1, maxN)
        q <- QC.chooseInt (1, maxQ)
        xs <- U.map Sum . U.fromList <$> QC.vectorOf n (QC.chooseInt rng)
        lrs <- U.fromList <$> QC.vectorOf q (rangeGen n)

        -- TODO: G.fold not in scope??
        let !expected = U.map (\(!l, !r) -> G.foldMap' id $ sliceLR l r xs) lrs

        let res = runST $ do
              seq <- newSS @(Sum Int) n
              root <- allocSeqSS seq xs
              (`evalStateT` root) $ do
                U.forM lrs $ \(!l, !r) -> do
                  root' <- get
                  (!m, !root'') <- foldSS seq root' l r
                  put root''
                  return m

        return . QC.counterexample (show (U.map getSum xs, lrs)) $ U.map getSum res QC.=== U.map getSum expected
    ]
  where
    maxN = 16
    maxQ = 16
    rng = (-20, 20)

tests :: [TestTree]
tests = [testGroup "Data.SplaySeq" [randomTests]]
