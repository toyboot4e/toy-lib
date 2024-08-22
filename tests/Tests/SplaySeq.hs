module Tests.SplaySeq where

import Algorithm.Bisect
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Core.SegmentAction
import Data.Foldable
import Data.Instances.Affine2d
import Data.Semigroup
import Data.Sequence qualified as Seq
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

type Seq s = SplaySeq s (Sum Int) (Sum Int)

type AffineSeq s = SplaySeq s (Sum Int) (Affine2d Int)

newSeq :: (PrimMonad m) => Int -> m (Seq (PrimState m))
newSeq = newSS

newAffineSeq :: (PrimMonad m) => Int -> m (AffineSeq (PrimState m))
newAffineSeq = newSS

clamp :: Int -> Int -> Int -> Int
clamp l r x
  | x < l = l
  | x > r = r
  | otherwise = x

-- | Reads from left to right and right to left.
readLR :: (PrimMonad m) => Seq (PrimState m) -> StateT SplayIndex m (U.Vector (Sum Int))
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

readInterval :: (PrimMonad m) => Seq (PrimState m) -> Int -> Int -> StateT SplayIndex m (U.Vector (Sum Int))
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

-- | Reads from left to right and right to left.
dynamicSequenceQueryGen :: Int -> Int -> (Int, Int) -> QC.Gen (U.Vector Int, U.Vector (Int, Int, Int, Int, Int))
dynamicSequenceQueryGen maxN maxQ rng = do
  n <- QC.chooseInt (1, maxN)
  q <- QC.chooseInt (1, maxQ)
  xs <- U.fromList <$> QC.vectorOf n (QC.chooseInt rng)
  qs <- U.replicateM q $ do
    t <- QC.chooseInt (0, 4)
    (!l, !r) <- rangeGen n
    i <- QC.chooseInt (0, n - 1)
    case t of
      0 -> do
        -- insert
        x <- QC.chooseInt rng
        return (0 :: Int, i, x, -1, -1)
      1 -> do
        -- delete
        return (1, i, -1, -1, -1)
      2 -> do
        -- reverse
        return (2, l, r, -1, -1)
      3 -> do
        -- affine
        b <- QC.chooseInt rng
        c <- QC.chooseInt rng
        return (3, l, r, b, c)
      4 -> do
        -- fold
        return (4, l, r, -1, -1)
      _ -> error "unreachable"
  return (xs, qs)

randomTests :: TestTree
randomTests =
  testGroup
    "SplaySeq random tests"
    [ QC.testProperty "SplaySeq: insert" $ do
        n <- QC.chooseInt (1, maxN)
        xs <- U.fromList <$> QC.vectorOf n (QC.chooseInt rng)
        let res = runST $ do
              seq <- newSeq n
              root <- allocSeqSS seq $ U.map Sum xs
              evalStateT (readLR seq) root
        return . QC.counterexample (show xs) $ (xs U.++ U.reverse xs) QC.=== U.map getSum res,
      QC.testProperty "SplaySeq: write" $ do
        n <- QC.chooseInt (1, maxN)
        xs <- U.fromList <$> QC.vectorOf n (QC.chooseInt rng)
        ys <- U.fromList <$> QC.vectorOf n (QC.chooseInt rng)
        let res = runST $ do
              seq <- newSeq n
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
              seq <- newSeq n
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
          seq <- lift $ newSeq n
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
            qs,
      QC.testProperty "SplaySeq-bisect" $ QCM.monadicIO $ do
        n <- QCM.pick (QC.chooseInt (1, maxN))
        let xs = U.generate n Sum
        boundary <- QCM.pick $ QC.chooseInt (0, n)

        let expected = bisectL 0 (n - 1) $ \i -> xs G.! i <= Sum boundary

        seq <- lift $ newSeq n
        root <- lift $ allocSeqSS seq xs
        (!_, !res) <- lift $ bisectLSS seq root (<= Sum boundary)

        QCM.assertWith (res == expected) $ show (res, expected),
      QC.testProperty "SplaySeq-sact" $ QCM.monadicIO $ do
        -- smaller version of this:
        -- https://judge.yosupo.jp/problem/dynamic_sequence_range_affine_range_sum
        (!xs, !qs) <- QCM.pick $ dynamicSequenceQueryGen maxN maxQ rng

        seq <- lift $ newAffineSeq (G.length xs + G.length qs)
        root0 <- lift $ allocSeqSS seq $ U.map Sum xs
        let pureSeq0 = Seq.fromList $ U.toList xs
        U.foldM'_
          ( \(!root, !acc) q -> case q of
              (0, !i_, !x, !_, !_) -> do
                -- insert
                let !i = max 0 $ clamp 0 (Seq.length acc - 1) i_
                let !acc' = Seq.insertAt i x acc
                root' <- insertSS seq root i $ Sum x

                return (root', acc')
              _ | Seq.length acc == 0 -> do
                return (root, acc)
              (1, !i_, !_, !_, !_) -> do
                -- delete
                let !i = clamp 0 (Seq.length acc - 1) i_
                let !acc' = Seq.deleteAt i acc
                !root' <- deleteSS seq root i

                return (root', acc')
              (2, !l_, r_, !_, !_) -> do
                -- reverse
                let !l = clamp 0 (Seq.length acc - 1) l_
                let !r = clamp 0 (Seq.length acc - 1) r_
                let !acc' =
                      let (!mid, !right) = Seq.splitAt (r + 1) acc
                          (!left, !mid') = Seq.splitAt l mid
                       in left Seq.>< Seq.reverse mid' Seq.>< right
                root' <- reverseSS seq root l r

                return (root', acc')
              (3, !l_, !r_, !b, !c) -> do
                -- apply affine transformation
                let !l = clamp 0 (Seq.length acc - 1) l_
                let !r = clamp 0 (Seq.length acc - 1) r_
                let !acc' =
                      let (!mid, !right) = Seq.splitAt (r + 1) acc
                          (!left, !mid') = Seq.splitAt l mid
                          !mid'' = (Affine2d (b, c) `segAct`) <$> mid'
                       in left Seq.>< mid'' Seq.>< right
                root' <- lift $ sactSS seq root l r $ Affine2d (b, c)

                return (root', acc')
              (4, !l_, !r_, !_, !_) -> do
                -- fold
                let !l = clamp 0 (Seq.length acc - 1) l_
                let !r = clamp 0 (Seq.length acc - 1) r_
                let !expected = sum $ map (Seq.index acc) [l .. r]
                (Sum !res, !root') <- lift $ foldSS seq root l r
                QCM.assertWith (res == expected) $ show ("fold", (l, r), res, expected)

                return (root', acc)
              _ -> error "unreachable"
          )
          (root0, pureSeq0)
          qs
    ]
  where
    maxN = 16
    maxQ = 16
    rng = (-20, 20)

tests :: [TestTree]
tests = [testGroup "Data.SplaySeq" [randomTests]]
