{-# LANGUAGE RecordWildCards #-}

-- | [Suffix Array](https://cp-algorithms.com/string/suffix-array.html) calculation.
--
-- = Definition
--
-- Suffix array is defined as @sa[i] = indexOf(sa[(n - 1 - i):])@ where @indexOf@ returns the index
-- after sort for all the suffixes.
module Data.ByteString.SuffixArray where

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.Bits
import qualified Data.ByteString.Char8 as BS
import Data.Char (chr, ord)
import Data.Ord (comparing)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VAI
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- | \(O(N^2)\) Suffix array calculation.
saOfNaive :: BS.ByteString -> U.Vector Int
saOfNaive bs =
  U.convert
    . V.map fst
    . V.modify (VAI.sortBy (comparing snd))
    $ V.generate n (\i -> (i, BS.drop (n - 1 - i) bs))
  where
    n = BS.length bs

-- vector<int> p(n), c(n), cnt(max(alphabet, n), 0);
-- for (int i = 0; i < n; i++)
--     cnt[s[i]]++;
-- for (int i = 1; i < alphabet; i++)
--     cnt[i] += cnt[i-1];
-- for (int i = 0; i < n; i++)
--     p[--cnt[s[i]]] = i;
-- c[p[0]] = 0;
-- int classes = 1;
-- for (int i = 1; i < n; i++) {
--     if (s[p[i]] != s[p[i-1]])
--         classes++;
--     c[p[i]] = classes - 1;
-- }

-- TODO: use `unsafeIndex`
-- TODO: is is faster to convert ByteString to Vector in preprocessing?
-- TODO: non-alphabet input?

-- | \(O(N)\) Auxiliary function to `saOf`.
sortCyclicShifts :: BS.ByteString -> (U.Vector Int, U.Vector Int)
sortCyclicShifts bs = (classes, perm)
  where
    !n = BS.length bs
    !alphabet = 255
    -- @p[i]@ is the index of the @i@ -th substring in the sorted order
    !perm = U.create $ do
      vec <- UM.replicate n (-1 :: Int)-- UM.unsafeNew n
      UM.unsafeWrite vec 0 (0 :: Int)
      -- TODO: reuse the `cnt` vector
      cnt <-
        U.unsafeThaw
          -- FIXME: smaller allocation as in the book??
          . U.scanl1' (+)
          . U.accumulate (+) (U.replicate alphabet (0 :: Int))
          . U.map ((,1) . ord)
          . U.fromList
          $ BS.unpack bs
      forM_ [0 .. n - 1] $ \i -> do
        let !c = BS.index bs i
        UM.modify cnt (subtract 1) (ord c)
        i' <- UM.read cnt i
        UM.unsafeWrite vec i' i
      return vec
    !classes =
      -- TODO: replace zipWith with scanl' for benchmarking
      U.scanl' (+) (0 :: Int) $
        U.zipWith
          (\i1 i2 -> if BS.index bs i1 == BS.index bs i2 then 0 else 1)
          (U.tail perm)
          perm

-- | \(O(N \log N)\) Suffix array calculation.
--
-- = The \(O(N \log N)\) algorithm
--
-- Binary lifting with smart sort.
saOf :: BS.ByteString -> U.Vector Int
saOf bs = U.tail $ lastClasses (bit 1 :: Int) classes0 perm0
  where
    !n = BS.length bs + 1
    !alphabet = 255
    -- zero character (null character in ASCII table)
    !c0 = chr 0
    (!classes0, !perm0) = sortCyclicShifts (BS.snoc bs c0)
    lastClasses :: Int -> U.Vector Int -> U.Vector Int -> U.Vector Int
    lastClasses len classes perm
      | len >= n = classes'
      | otherwise = lastClasses (len .<<. 1) classes' perm'
      where
        fastMod n i
          | i < 0 = i + n
          | otherwise = i
        -- this sort in details
        perm' = U.imap (\i p -> fastMod n (p - bit i)) perm
        nClass = U.last classes + 1
        classes' = runST $ do
          -- TODO: reuse cnt vec
          -- TODO: in-place update
          -- TODO: why perm''??
          let !perm'' = U.create $ do
                cnt <-
                  U.unsafeThaw
                    . U.scanl1' (+)
                    -- FIXME: the length can be `nClass`?
                    . U.accumulate (+) (U.replicate alphabet (0 :: Int))
                    $ U.map ((,1) . ord . BS.index bs) perm'
                vec <- UM.replicate nClass (0 :: Int)
                U.forM_ perm' $ \i -> do
                  let !c = U.unsafeIndex classes i
                  UM.modify cnt (subtract 1) c
                  i' <- UM.read cnt c
                  UM.unsafeWrite vec i' i
                return vec
          return
            . U.scanl1' (+)
            $ U.zipWith
              ( \i1 i2 ->
                  let !c11 = U.unsafeIndex classes i1
                      !c12 = U.unsafeIndex classes . fastMod n $ i1 + len
                      !c21 = U.unsafeIndex classes i2
                      !c22 = U.unsafeIndex classes . fastMod n $ i2 + len
                   in if c11 == c21 && c12 == c22
                        then 0
                        else 1
              )
              (U.tail perm'')
              perm''

