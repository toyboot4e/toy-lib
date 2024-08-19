module Tests.SplaySeq where

import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Semigroup
import Data.SplaySeq
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Debug.Trace
import Test.QuickCheck.Monadic qualified as QCM
import Test.Tasty
import Test.Tasty.QuickCheck qualified as QC
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

readInterval :: (PrimMonad m) => SplaySeq (PrimState m) (Sum Int) -> Int -> Int -> StateT SplayIndex m (U.Vector (Sum Int))
readInterval seq l r = do
  let !n = capacitySS seq
  U.generateM (r + 1 - l) $ \i_ -> do
    root' <- get
    (!root'', !x) <- readSS seq root' (i_ + l)
    put root''
    return x

-- | Reads from left to right and right to left.
queryGen :: Int -> QC.Gen (Int, Int, Int)
queryGen n = do
  t <- QC.chooseInt (0, 1)
  (!l, !r) <- rangeGen n
  return (t, l, r)

-- | Reads from left to right and right to left.
problemGen :: Int -> Int -> (Int, Int) -> QC.Gen (Int, Int, U.Vector Int, U.Vector (Int, Int, Int))
problemGen maxN maxQ rng = do
  n <- QC.chooseInt (1, maxN)
  q <- QC.chooseInt (1, maxQ)
  xs <- U.fromList <$> QC.vectorOf n (QC.chooseInt rng)
  qs <- U.fromList <$> QC.vectorOf q (queryGen n)
  return (n, q, xs, qs)

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

        return . QC.counterexample (show (U.map getSum xs, lrs)) $ U.map getSum res QC.=== U.map getSum expected,
      QC.testProperty "SplaySeq-reverse" $ QCM.monadicIO $ do
        QCM.forAllM (problemGen maxN maxQ rng) $ \(!n, !q, !xs, !qs) -> do
          vec <- lift $ U.thaw $ U.map Sum xs
          seq <- lift $ newSS @(Sum Int) n
          root0 <- lift $ allocSeqSS seq $ U.map Sum xs

          let testFold root l r = do
                (!m1, !root') <- lift $ foldSS seq root l r
                m2 <- U.foldl' (<>) mempty <$> lift (U.unsafeFreeze (GM.slice l (r + 1 - l) vec))
                QCM.assertWith (m1 == m2) $ show (m1, m2)
                return root'

          let testMatch root l r = do
                (!ys1, !root') <- runStateT (readInterval seq l r) root
                ys2 <- lift $ U.unsafeFreeze $ GM.slice l (r + 1 - l) vec
                QCM.assertWith (ys1 == ys2) $ show (ys1, ys2)
                return root'

          U.foldM'_
            ( \root (!t, !l, !r) -> do
                if t == 0
                  then do
                    -- reverse
                    GM.reverse $ GM.slice l (r + 1 - l) vec
                    !root' <- lift $ reverseSS seq root l r
                    return root'
                  else do
                    -- evaluate
                    root' <- testFold root l r
                    root'' <- testMatch root' l r
                    return root''
            )
            root0
            qs
    ]
  where
    maxN = 16
    maxQ = 16
    rng = (-20, 20)

tests :: [TestTree]
tests = [testGroup "Data.SplaySeq" [randomTests]]
