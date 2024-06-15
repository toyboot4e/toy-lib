-- | [Suffix Array](https://cp-algorithms.com/string/suffix-array.html) calculation.
--
-- = Definition
--
-- \(\mathcal{sa}[i] = \mathcal{originalOrderOf}(\mathcal{sa}[(n-1-i):])\) where
-- \(\mathcal{originalOrderOf(i)}\) returns the order of i-th suffix.
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

-- | \(O(N^2 \log N)\) Suffix array calculation.
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
--     ab   ba   a$   $a
--   aba$ ba$a a$ab $aba
-- @
saOf :: BS.ByteString -> U.Vector Int
saOf bs0 = G.tail $ sortCyclicShifts (BS.snoc bs0 c0)
  where
    -- Zero character (null character in ASCII table)
    !c0 = chr 0

-- TODO: non-ascii input? (compress in pre-processing?)
-- TODO: Efficiency
-- - TODO: is is faster to convert ByteString to Vector in preprocessing?
-- - TODO: use `unsafeIndex`
-- - TODO: reuse the `cnt` and `perm'` vector
-- - TODO: Is StateT slow?
-- TODO: SA-IS

-- | \(O(N \log N)\) Sorts cyclic substrings of @bs@ of length @n@.
sortCyclicShifts :: BS.ByteString -> U.Vector Int
sortCyclicShifts bs = sortCyclicShifts' n 1 nClasses0 classes0 perm0
  where
    (!nClasses0, !classes0, !perm0) = sortByCharacter bs
    !n = BS.length bs

-- | \(O(N)\) Preprocessing function to `sortCyclicShifts`.
sortByCharacter :: BS.ByteString -> (Int, U.Vector Int, U.Vector Int)
sortByCharacter bs = (nClasses, classes, perm)
  where
    !n = BS.length bs
    !alphabet = 256

    -- Sort the characters using counting sort.
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
      -- Indices are reversed for stable sorting. It doesn't make any difference after all, but
      -- it's for consistency with the `sortCyclicShifts'` implementation below.
      forM_ [n - 1, n - 2 .. 0] $ \i -> do
        let !c = ord $ BS.index bs i
        GM.modify cnt (subtract 1) c
        i' <- GM.read cnt c
        GM.write vec i' i
      return vec

    -- Record equal character classes and assign them to the characters.
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
      (nClasses,) <$> G.unsafeFreeze vec

-- | \(O(N \log N)\) Binary lifting part of `sortCyclicShifts`.
sortCyclicShifts' :: Int ->Int -> Int -> U.Vector Int -> U.Vector Int -> U.Vector Int
sortCyclicShifts' n len nClasses classes perm
  | len >= n = perm
  | otherwise =
      let (!nClasses', !classes') = getNextClasses ()
       in sortCyclicShifts' n (len .<<. 1) nClasses' classes' perm'
  where
    -- Helpers
    fastAddMod m x y
      | x' >= m = x' - m
      | otherwise = x'
      where
        !x' = x + y

    fastSubMod m x y
      | x' < 0 = x' + m
      | otherwise = x'
      where
        !x' = x - y

    -- In the original index (perm[i]), go forward @len@ characters. This is where the left half
    -- of the new substring is at (see also the diagram in `saOf`):
    rightHalves = G.map (\p -> fastSubMod n p len) perm

    -- Sort by the left halves of the substrings using counting sort.
    perm' = U.create $ do
      cnt <-
        U.unsafeThaw
          . G.scanl1' (+)
          . G.accumulate (+) (G.replicate nClasses (0 :: Int))
          $ G.map (\i -> (classes G.! i, 1)) rightHalves

      vec <- UM.unsafeNew n
      GM.write vec (G.head rightHalves) 0
      -- The `reverse` is for stable sorting, which preserves the result of the last sort by
      -- the right halves of the substrings.
      G.forM_ (G.reverse rightHalves) $ \i -> do
        let !c = classes G.! i
        GM.modify cnt (subtract 1) c
        i' <- GM.read cnt c
        GM.write vec i' i
      return vec

    -- Record equal substring classes and assign them to the substrings.
    getNextClasses () = runST $ do
      vec <- UM.unsafeNew n
      GM.write vec (G.head perm') 0
      !nClasses' <-
        fmap (+ 1) . (`execStateT` (0 :: Int)) $
          G.zipWithM_
            ( \i1 i2 -> do
                let !l1 = (G.!) classes i1
                    !r1 = (G.!) classes $ fastAddMod n i1 len
                    !l2 = (G.!) classes i2
                    !r2 = (G.!) classes $ fastAddMod n i2 len
                unless (l1 == l2 && r1 == r2) $ do
                  modify' (+ 1)
                GM.write vec i1 =<< get
            )
            (G.tail perm')
            perm'
      (nClasses',) <$> G.unsafeFreeze vec
