{-# LANGUAGE LambdaCase #-}

module Tests.Slide where

import Control.Monad (when)
import Data.Buffer
import Data.Core.SemigroupAction
import Data.Instances.Affine2d
import Data.Semigroup
import Data.Slide
import Data.Vector.Unboxed qualified as U
import Test.QuickCheck.Monadic qualified as QCM
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Debug.Trace

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

props :: TestTree
props =
  testGroup
    "SlidingFold properties"
    [ QC.testProperty "slide-stack" $ QCM.monadicIO $ do
        q <- QCM.pick $ QC.chooseInt (1, maxQ)
        qs <- QCM.pick $ U.fromList <$> QC.vectorOf q (gen1 q)
        buf <- QCM.run $ newBuffer q
        window <- QCM.run $ newSF q

        U.forM_ qs $ \case
          (0, !a, !b) -> do
            -- push back
            QCM.run $ pushBack buf . Dual $ Affine2d (a, b)
            QCM.run $ pushBackSF window . Dual $ Affine2d (a, b)
          (1, !_, !_) -> do
            -- pop froot
            len <- QCM.run $ lengthBuffer buf
            when (len > 0) $ do
              QCM.run $ popFront_ buf
              QCM.run $ popFrontSF window
          (2, !x, !_) -> do
            -- fold
            expected <- (`sact` x) . getDual <$> QCM.run (U.foldl' (<>) mempty <$> unsafeFreezeBuffer buf)
            res <- (`sact` x) . getDual <$> QCM.run (foldSF window)
            let !_ = traceShow res
            QCM.assertWith (res == expected) $ show (res, expected)
    ]
  where
    maxQ = 64

tests :: [TestTree]
tests = [testGroup "Data.Slide" [props]]
