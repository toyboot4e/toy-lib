{-# LANGUAGE RecordWildCards #-}

-- | Points on a 2D plane with monoids. The monoid values can be altered dynamically in
-- \(O(\log N)\) and rectangle folding can be performed in \(O(\log^2 N)\).
--
-- = Typical problems
-- - [Rectangle Sum](https://judge.yosupo.jp/problem/rectangle_sum)
-- - [Point Add Rectangle Sum](https://judge.yosupo.jp/problem/point_add_rectangle_sum)
-- - [Static Range Count Distinct](https://judge.yosupo.jp/problem/static_range_count_distinct)
--
-- FIXME: My wavelet matrix is somehow slow.
module Data.WaveletMatrix.SegTree where

import Algorithm.Bisect
import Control.Monad.Primitive
import Data.Bit
import Data.Bits
import Data.Core.Group
import Data.Maybe
import Data.SegmentTree.Strict
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VAI
import Data.Vector.Extra (bindex)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import Data.WaveletMatrix.BitVector
import Data.WaveletMatrix.Raw

-- | Segment Tree on Wavelet Matrix: points on a 2D plane and rectangle folding.
data WaveletMatrixSegTree s a = WaveletMatrixSegTree
  { -- | The wavelet matrix that represents points on a 2D plane.
    rawWmWMST :: !RawWaveletMatrix,
    -- | (x, y) index compression.
    xysWMST :: !(U.Vector (Int, Int)),
    -- | y index compression.
    ysWMST :: !(U.Vector Int),
    -- | The segment tree of the weights of the points in order of `xysWMST`.
    segTreesWMST :: !(V.Vector (SegmentTree s a))
  }

-- | \(O(N (\log N + \log A)\) Creates a 2D wavelet matrix.
{-# INLINE buildWMST #-}
buildWMST :: (Monoid a, U.Unbox a, PrimMonad m) => U.Vector (Int, Int) -> m (WaveletMatrixSegTree (PrimState m) a)
buildWMST xys = do
  let !xysWMST = U.uniq . U.modify VAI.sort $ xys
  let !ysWMST = U.uniq . U.modify VAI.sort $ U.map (\(!_, !y) -> y) xys
  let !n = G.length xysWMST
  -- REMARK: Be sure to use `n + 1` because the folding cannot handle the case yUpper is `2^{height}`.
  let !rawWmWMST = buildRWM (n + 1) $ U.map (\(!_, !y) -> bindex ysWMST y) xysWMST
  -- modifycations are delayed so that the user can follow the change over time.
  segTreesWMST <- V.replicateM (heightRWM rawWmWMST) (newSTree n)
  pure WaveletMatrixSegTree {..}

-- | \(O(N (\log N + \log A)\) Modifies a point. Access to unknown points are undefined.
{-# INLINE modifyWMST #-}
modifyWMST :: (Monoid a, U.Unbox a, PrimMonad m) => WaveletMatrixSegTree (PrimState m) a -> (a -> a) -> (Int, Int) -> m ()
modifyWMST WaveletMatrixSegTree {..} f (!x, !y) = stToPrim $ do
  let !i_ = fromJust $ bsearchL xysWMST (<= (x, y))
  V.ifoldM'_
    ( \ !i !iRow (!bits, !stree) -> do
        let !i0 = freq0BV bits i
        let !i'
              | unBit $ G.unsafeIndex (bitsBV bits) i =
                  i + nZerosRWM rawWmWMST G.! iRow - i0
              | otherwise = i0
        modifySTree stree f i'
        pure i'
    )
    i_
    $ V.zip (bitsRWM rawWmWMST) segTreesWMST

-- | \(O(\log^2 N)\) Folding.
{-# INLINE _foldLTWMST #-}
_foldLTWMST :: (Monoid a, U.Unbox a, PrimMonad m) => WaveletMatrixSegTree (PrimState m) a -> Int -> Int -> Int -> m a
_foldLTWMST WaveletMatrixSegTree {..} !l_ !r_ yUpper = stToPrim $ do
  (!res, !_, !_) <- do
    V.ifoldM'
      ( \(!acc, !l, !r) !iRow (!bits, !stree) -> do
          let !l0 = freq0BV bits l
              !r0 = freq0BV bits r
          -- REMARK: The function cannot handle the case yUpper = N = 2^i. See the constructor for
          -- how it's handled and note that l_ and r_ are compressed indices.
          if testBit yUpper (heightRWM rawWmWMST - 1 - iRow)
            then do
              acc' <- (acc <>) <$> foldSTree stree l0 (r0 - 1)
              let !l' = l + nZerosRWM rawWmWMST G.! iRow - l0
              let !r' = r + nZerosRWM rawWmWMST G.! iRow - r0
              pure (acc', l', r')
            else do
              pure (acc, l0, r0)
      )
      (mempty, l_, r_ + 1)
      $ V.zip (bitsRWM rawWmWMST) segTreesWMST
  pure res

-- | \(O(\log^2 N)\) Folding.
{-# INLINE foldMayWMST #-}
foldMayWMST :: (Group a, U.Unbox a, PrimMonad m) => WaveletMatrixSegTree (PrimState m) a -> Int -> Int -> Int -> Int -> m (Maybe a)
foldMayWMST wm@WaveletMatrixSegTree {..} !xl !xr !yl !yr
  | not $ 0 <= xl' && xl' <= xr' && xr' < G.length xysWMST = pure Nothing
  | not $ 0 <= yl' && yl' <= yr' && yr' < G.length ysWMST = pure Nothing
  | otherwise = do
      s1 <- _foldLTWMST wm xl' xr' (yr' + 1)
      s2 <- _foldLTWMST wm xl' xr' yl'
      pure . Just $ s1 <> invert s2
  where
    !n = lengthRWM rawWmWMST
    !xl' = fromMaybe n $ bisectR 0 (G.length xysWMST - 1) $ \i -> (< xl) . fst $ xysWMST G.! i
    !xr' = fromMaybe (-1) $ bisectL 0 (G.length xysWMST - 1) $ \i -> (<= xr) . fst $ xysWMST G.! i
    !yl' = fromMaybe n $ bisectR 0 (G.length ysWMST - 1) $ \i -> (< yl) $ ysWMST G.! i
    !yr' = fromMaybe (-1) $ bisectL 0 (G.length ysWMST - 1) $ \i -> (<= yr) $ ysWMST G.! i

-- TODO: monoid folding.

-- | \(O(\log N)\) Index restoration. Access to unknown points are undefined.
{-# INLINE indexXWMST #-}
indexXWMST :: WaveletMatrixSegTree s a -> Int -> Int
indexXWMST WaveletMatrixSegTree {xysWMST} x =
  maybe (error "cannot index x") (fst . (xysWMST G.!)) $ bsearchL xysWMST ((<= x) . fst)

-- | \(O(\log N)\) Index restoration. Access to unknown points are undefined.
{-# INLINE indexYWMST #-}
indexYWMST :: WaveletMatrixSegTree s a -> Int -> Int
indexYWMST WaveletMatrixSegTree {ysWMST} y =
  maybe (error "cannot index y") (ysWMST G.!) $ bsearchL ysWMST (<= y)

-- | \(O(\log N)\) Index restoration. Access to unknown points are undefined.
{-# INLINE indexXYWMST #-}
indexXYWMST :: WaveletMatrixSegTree s a -> Int -> Int -> (Int, Int)
indexXYWMST WaveletMatrixSegTree {xysWMST} x y =
  maybe (error "cannot index (x, y)") (xysWMST G.!) $ bsearchL xysWMST (<= (x, y))
