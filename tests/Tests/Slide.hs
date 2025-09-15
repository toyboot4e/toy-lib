{-# LANGUAGE LambdaCase #-}

module Tests.Slide where

import Control.Monad (when)
import Data.Buffer
import Data.Core.SemigroupAction
import Data.Instances.Affine1
import Data.Semigroup
import Data.Slide
import Data.Vector.Unboxed qualified as U
import Debug.Trace
import Test.QuickCheck.Monadic qualified as QCM
import Test.Tasty
import Test.Tasty.QuickCheck as QC

gen1 :: Int -> Gen (Int, Int, Int)
gen1 q = do
  t <- QC.chooseInt (0, 2)
  case t of
    0 -> do
      a <- QC.chooseInt rng
      b <- QC.chooseInt rng
      return (0, a, b)
    1 -> do
      return (1, -1, -1)
    2 -> do
      x <- QC.chooseInt rng
      return (2, x, -1)
    _ -> error "unreachable"
  where
    rng = (-32, 32)

gen2 :: Int -> Gen (Int, Int, Int)
gen2 q = do
  t <- QC.chooseInt (0, 4)
  case t of
    0 -> do
      a <- QC.chooseInt rng
      b <- QC.chooseInt rng
      return (0, a, b)
    1 -> do
      a <- QC.chooseInt rng
      b <- QC.chooseInt rng
      return (1, a, b)
    2 -> do
      return (2, -1, -1)
    3 -> do
      return (3, -1, -1)
    4 -> do
      x <- QC.chooseInt rng
      return (4, x, -1)
    _ -> error "unreachable"
  where
    rng = (-32, 32)

props :: TestTree
props =
  testGroup
    "SlidingFold properties"
    [ QC.testProperty "slide-stack" $ QCM.monadicIO $ do
        q <- QCM.pick $ QC.chooseInt (1, maxQ)
        qs <- QCM.pick $ U.fromList <$> QC.vectorOf q (gen1 q)
        buf <- QCM.run $ newBuffer q
        window <- QCM.run $ newSSF q

        U.forM_ qs $ \case
          (0, !a, !b) -> do
            -- push back
            QCM.run $ pushBack buf . Dual $ Affine1 (a, b)
            QCM.run $ pushBackSSF window . Dual $ Affine1 (a, b)
          (1, !_, !_) -> do
            -- pop froot
            len <- QCM.run $ lengthBuffer buf
            when (len > 0) $ do
              QCM.run $ popFront_ buf
              QCM.run $ popFrontSSF window
          (2, !x, !_) -> do
            -- fold
            expected <- (`sact` x) . getDual <$> QCM.run (U.foldl' (<>) mempty <$> unsafeFreezeBuffer buf)
            res <- (`sact` x) . getDual <$> QCM.run (foldSSF window)
            let !_ = traceShow res
            QCM.assertWith (res == expected) $ show (res, expected),
      QC.testProperty "slide-deque" $ QCM.monadicIO $ do
        q <- QCM.pick $ QC.chooseInt (1, maxQ)
        qs <- QCM.pick $ U.fromList <$> QC.vectorOf q (gen2 q)
        buf <- QCM.run $ newBufferAsDeque q
        window <- QCM.run $ newDSF q

        U.forM_ qs $ \case
          (0, !a, !b) -> do
            -- push front
            QCM.run $ pushFront buf . Dual $ Affine1 (a, b)
            QCM.run $ pushFrontDSF window . Dual $ Affine1 (a, b)
          (1, !a, !b) -> do
            -- push back
            QCM.run $ pushBack buf . Dual $ Affine1 (a, b)
            QCM.run $ pushBackDSF window . Dual $ Affine1 (a, b)
          (2, !_, !_) -> do
            -- pop froot
            len <- QCM.run $ lengthBuffer buf
            when (len > 0) $ do
              QCM.run $ popFront_ buf
              QCM.run $ popFrontDSF window
          (3, !_, !_) -> do
            -- pop back
            len <- QCM.run $ lengthBuffer buf
            when (len > 0) $ do
              QCM.run $ popBack_ buf
              QCM.run $ popBackDSF window
          (4, !x, !_) -> do
            -- fold
            expected <- (`sact` x) . getDual <$> QCM.run (U.foldl' (<>) mempty <$> unsafeFreezeBuffer buf)
            res <- (`sact` x) . getDual <$> QCM.run (foldDSF window)
            QCM.assertWith (res == expected) $ show (res, expected)
    ]
  where
    maxQ = 16

tests :: [TestTree]
tests = [testGroup "Data.Slide" [props]]
