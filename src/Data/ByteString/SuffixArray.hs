-- | [Suffix Array](https://cp-algorithms.com/string/suffix-array.html) calculation.
--
-- = Definition
--
-- Suffix array is defined as @sa[i] = indexOf(sa[(n - 1 - i):])@ where @indexOf@ returns the index
-- after sort for all the suffixes.
module Data.ByteString.SuffixArray where

import Control.Monad (forM_, unless, when)
import Control.Monad.ST (runST)
import Control.Monad.Trans.State.Strict (execStateT, get, modify')
import Data.Bits
import qualified Data.ByteString.Char8 as BS
import Data.Char (chr, ord)
import Data.Ord (comparing)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VAI
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- | \(O(N^2)\) Suffix array calculation.
saOfNaive :: BS.ByteString -> U.Vector Int
saOfNaive bs =
  U.convert
    . V.map fst
    . V.modify (VAI.sortBy (comparing snd))
    $ V.generate n (\i -> (i, BS.drop i bs))
  where
    n = BS.length bs

-- TODO: use `unsafeIndex`
-- TODO: is is faster to convert ByteString to Vector in preprocessing?
-- TODO: non-alphabet input?

-- | \(O(N)\) Auxiliary function to `saOf`.
sortCyclicShifts :: BS.ByteString -> (Int, U.Vector Int, U.Vector Int)
sortCyclicShifts bs = (nClasses, classes, perm)
  where
    !n = BS.length bs
    !alphabet = 256
    -- @p[i]@ is the index of the @i@ -th substring in the sorted order
    !perm = U.create $ do
      vec <- UM.unsafeNew n
      -- TODO: reuse the `cnt` vector
      cnt <-
        U.unsafeThaw
          -- FIXME: smaller allocation as in the book??
          . G.scanl1' (+)
          . G.accumulate (+) (U.replicate alphabet (0 :: Int))
          . G.map ((,1) . ord)
          . U.fromList
          $ BS.unpack bs
      forM_ [0 .. n - 1] $ \i -> do
        let !c = ord $ BS.index bs i
        GM.modify cnt (subtract 1) c
        i' <- GM.read cnt c
        GM.write vec i' i
      return vec
    (!nClasses, !classes) = runST $ do
      vec <- UM.unsafeNew n
      GM.write vec (perm G.! 0) 0
      -- why drop 1
      !nClasses <-
        fmap (+ 1) . (`execStateT` (0 :: Int)) $
          G.zipWithM_
            ( \i1 i2 -> do
                when (BS.index bs i1 /= BS.index bs i2) $ do
                  modify' (+ 1)
                GM.write vec i1 =<< get
            )
            (G.tail perm)
            perm
      (nClasses,) <$> U.unsafeFreeze vec

-- | \(O(N \log N)\) Suffix array calculation.
--
-- = The \(O(N \log N)\) algorithm
--
-- Binary lifting with smart sort.
saOf :: BS.ByteString -> U.Vector Int
-- TODO: why start with one??
saOf bs0 = G.tail $ lastPerm 1 nClasses0 classes0 perm0
  where
    !c0 = chr 0
    !bs = BS.snoc bs0 c0
    !n = BS.length bs
    -- zero character (null character in ASCII table)
    (!nClasses0, !classes0, !perm0) = sortCyclicShifts bs
    lastPerm :: Int -> Int -> U.Vector Int -> U.Vector Int -> U.Vector Int
    lastPerm len nClasses classes perm
      | len >= n = perm
      | otherwise = lastPerm (len .<<. 1) nClasses' classes' perm''
      where
        -- this sort in details
        perm' = G.map (\p -> fastMod1 n (p - len)) perm
          where
            fastMod1 n i
              | i < 0 = i + n
              | otherwise = i
        perm'' = U.create $ do
          cnt <-
            U.unsafeThaw
              . G.scanl1' (+)
              . G.accumulate (+) (G.replicate nClasses (0 :: Int))
              $ G.map (\i -> (classes G.! i, 1)) perm'
          vec <- UM.replicate n (-1 :: Int)
          GM.write vec (G.head perm') 0
          -- TODO: no reverse?
          G.forM_ (G.reverse perm') $ \i -> do
            let !c = classes G.! i
            GM.modify cnt (subtract 1) c
            i' <- GM.read cnt c
            GM.write vec i' i
          return vec
        (!nClasses', !classes') = runST $ do
          -- TODO: reuse cnt vec
          -- TODO: in-place update

          vec <- UM.unsafeNew n
          GM.write vec (G.head perm'') 0
          !nClasses' <-
            fmap (+ 1) $
              (`execStateT` (0 :: Int)) $
                G.zipWithM_
                  ( \i1 i2 -> do
                      let !c11 = (G.!) classes i1
                          !c12 = (G.!) classes . fastMod2 n $ i1 + len
                          !c21 = (G.!) classes i2
                          !c22 = (G.!) classes . fastMod2 n $ i2 + len
                      unless (c11 == c21 && c12 == c22) $ do
                        modify' (+ 1)
                      GM.write vec i1 =<< get
                  )
                  (G.tail perm'')
                  perm''
          (nClasses',) <$> U.unsafeFreeze vec
          where
            fastMod2 n i
              | i >= n = i - n
              | otherwise = i
