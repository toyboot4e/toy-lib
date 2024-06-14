-- | [Suffix Array](https://cp-algorithms.com/string/suffix-array.html) calculation.
--
-- = Definition
--
-- Suffix array is defined as @sa[i] = indexOf(sa[(n - 1 - i):])@ where @indexOf@ returns the i-th
-- substring in the sorted order.
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

-- | \(O(N \log N)\) Suffix array calculation.
--
-- = The \(O(N \log N)\) algorithm
--
-- Binary lifting with smart sort.
--
-- @
--      a    b    a    $
--     $a   ab   ba   a$
--   ba$a a$ab aba$ $aba
-- @
saOf :: BS.ByteString -> U.Vector Int
saOf bs0 = sortCyclicShifts (BS.snoc bs0 c0)
  where
    -- zero character (null character in ASCII table)
    !c0 = chr 0

-- TODO: use `unsafeIndex`
-- TODO: is is faster to convert ByteString to Vector in preprocessing?
-- TODO: non-alphabet input?
-- TODO: reuse the `cnt` and `perm'` vector
-- TODO: Is StateT slow?

-- | \(O(N)\) Preprocessing function to `sortCyclicShifts`.
sortByCharacter :: BS.ByteString -> (Int, U.Vector Int, U.Vector Int)
sortByCharacter bs = (nClasses, classes, perm)
  where
    !n = BS.length bs
    !alphabet = 256

    -- sort the characters using counting sort.
    !perm = U.create $ do
      cnt <-
        U.unsafeThaw
          -- TODO: smaller allocation in non-ascii input?
          . G.scanl1' (+)
          . G.accumulate (+) (U.replicate alphabet (0 :: Int))
          . G.map ((,1) . ord)
          . U.fromList
          $ BS.unpack bs
      vec <- UM.unsafeNew n
      forM_ [0 .. n - 1] $ \i -> do
        let !c = ord $ BS.index bs i
        GM.modify cnt (subtract 1) c
        i' <- GM.read cnt c
        GM.write vec i' i
      return vec

    -- record equal character classes and assign them to the characters.
    (!nClasses, !classes) = runST $ do
      vec <- UM.unsafeNew n
      GM.write vec (G.head perm) 0
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

-- | Sort cyclic substrings of length @no.
sortCyclicShifts :: BS.ByteString -> U.Vector Int
sortCyclicShifts bs = lastPerm 1 nClasses0 classes0 perm0
  where
    !n = BS.length bs
    (!nClasses0, !classes0, !perm0) = sortByCharacter bs

    fastAddMod m x y
      | x' >= m = x - m
      | otherwise = x'
      where
        !x' = x + y

    fastSubMod m x y
      | x' < 0 = x' + m
      | otherwise = x'
      where
        !x' = x - y

    lastPerm :: Int -> Int -> U.Vector Int -> U.Vector Int -> U.Vector Int
    lastPerm len nClasses classes perm
      | len >= n = perm
      | otherwise =
        let (!nClasses', classes') = getNextClasses ()
         in lastPerm (len .<<. 1) nClasses' classes' perm'
      where
        -- In the original index (perm[i]), move back @len@ characters. This is where the left half
        -- of the new substring is at (see also the above diagram):
        leftHalves = G.map (\p -> fastSubMod n p len) perm

        -- sort by the left halves of the substrings using counting sort.
        perm' = U.create $ do
          cnt <-
            U.unsafeThaw
              . G.scanl1' (+)
              . G.accumulate (+) (G.replicate nClasses (0 :: Int))
              $ G.map (\i -> (classes G.! i, 1)) perm'

          vec <- UM.unsafeNew n
          GM.write vec (G.head leftHalves) 0
          -- TODO: no reverse?
          G.forM_ (G.reverse leftHalves) $ \i -> do
            let !c = classes G.! i
            GM.modify cnt (subtract 1) c
            i' <- GM.read cnt c
            GM.write vec i' i
          return vec

        -- record equal substring classes and assign them to the substrings.
        getNextClasses () = runST $ do
          vec <- UM.unsafeNew n
          GM.write vec (G.head perm') 0
          !nClasses' <-
            fmap (+ 1) $
              (`execStateT` (0 :: Int)) $
                G.zipWithM_
                  ( \i1 i2 -> do
                      let !c11 = (G.!) classes i1
                          !c12 = (G.!) classes $ fastAddMod n i1 len
                          !c21 = (G.!) classes i2
                          !c22 = (G.!) classes $ fastAddMod n i2 len
                      unless (c11 == c21 && c12 == c22) $ do
                        modify' (+ 1)
                      GM.write vec i1 =<< get
                  )
                  (G.tail perm')
                  perm'
          (nClasses',) <$> U.unsafeFreeze vec
