-- | Typical DP utilities
--
-- TODO: Refactor `relaxMany` variants.
module ToyLib.DP where

import Control.Monad.ST
import Data.BitSet (powersetVU)
import Data.Bits
import Data.Bool (bool)
import Data.Ix
import Data.Maybe
import Data.SegmentTree.Strict
import Data.Unindex
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import Data.Vector.IxVector
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Stack (HasCallStack)
import ToyLib.Prelude (rangeVU, repM_)

-- | Variant of `U.constructN`.
constructFor :: (U.Unbox a, U.Unbox b) => a -> U.Vector b -> (U.Vector a -> b -> a) -> U.Vector a
constructFor !x0 !input !f = U.create $ do
  !vec <- UM.unsafeNew (U.length input + 1)
  UM.unsafeWrite vec 0 x0

  flip U.imapM_ input $ \lenS1 x -> do
    !vec' <- U.take (succ lenS1) <$> U.unsafeFreeze vec
    UM.unsafeWrite vec (succ lenS1) $! f vec' x

  return vec

-- | `accumulate` variant with `concatMap`-like expander. Be wanrned that *the @input@ is consumed
-- in-place*. Run like @relaxMany vec0 (U.force vec0) $ \x -> ..@ if it needs to be cloned.
--
-- @relaxMany !f !vec0 !input !expander@ ~ @VG.accumulate f vec0 $ VG.concatMap expander input@
relaxMany :: (HasCallStack, VG.Vector v a, VG.Vector v (Int, a), VG.Vector v b) => (a -> a -> a) -> v a -> v b -> (b -> v (Int, a)) -> v a
relaxMany !relax !vec0 !input !expander = VG.create $ do
  !vec <- VG.unsafeThaw vec0

  VG.forM_ input $ \x -> do
    VG.forM_ (expander x) $ \(!i, !x') -> do
      VGM.modify vec (`relax` x') i

  return vec

-- | `relaxMany` with index input. Be wanrned that *the @input@ is consumed in-place*. Run like
-- @relaxMany vec0 (U.force vec0) $ \x -> ..@ if it needs to be cloned.
irelaxMany :: (HasCallStack, VG.Vector v a, VG.Vector v (Int, a), VG.Vector v b) => (a -> a -> a) -> v a -> v b -> (Int -> b -> v (Int, a)) -> v a
irelaxMany !relax !vec0 !input !expander = VG.create $ do
  !vec <- VG.unsafeThaw vec0

  VG.iforM_ input $ \ix x -> do
    VG.forM_ (expander ix x) $ \(!i, !x') -> do
      VGM.modify vec (`relax` x') i

  return vec

-- | Monoid variant of `relaxMany`
relaxMany' :: (Monoid m, U.Unbox m, U.Unbox a) => U.Vector m -> U.Vector a -> (a -> U.Vector (Int, m)) -> U.Vector m
relaxMany' !vec0 !input !expander = U.create $ do
  !vec <- U.unsafeThaw vec0

  U.forM_ input $ \x -> do
    U.forM_ (expander x) $ \(!i, !x') -> do
      UM.modify vec (<> x') i

  return vec

{-# INLINE pushBasedConstructN #-}
pushBasedConstructN :: (HasCallStack, VG.Vector v a, VG.Vector v (Int, a)) => (a -> a -> a) -> v a -> (Int -> v a -> v (Int, a)) -> v a
pushBasedConstructN !relax !vec0 !expander = VG.create $ do
  !vec <- VG.unsafeThaw vec0

  repM_ 0 (VGM.length vec - 1) $ \iFrom -> do
    -- REMARK: Because this is a push-based DP, the value for the interested index is already known.
    !freezed <- VG.unsafeFreeze (VGM.take (iFrom + 1) vec)
    VG.forM_ (expander iFrom freezed) $ \(!iTo, !x') -> do
      VGM.modify vec (`relax` x') iTo

  return vec

-- | Returns non-zero two spans over the given inclusive range @[l, r]@.
-- >>> spansVU 3 6
-- [((3,3),(4,6)),((3,4),(5,6)),((3,5),(6,6))]
spansVU :: Int -> Int -> U.Vector ((Int, Int), (Int, Int))
spansVU !l !r = U.map (\len -> ((l, l + len - 1), (l + len, r))) $ rangeVU 1 (r - l)

-- | `U.constructN` for `IxVector`
constructIV :: (Unindex i, U.Unbox a) => (i, i) -> (IxVector i (U.Vector a) -> i -> a) -> IxVector i (U.Vector a)
constructIV !rng !f = IxVector rng $ VG.constructN (rangeSize rng) $ \vec ->
  let !i = unindex rng (VG.length vec)
   in f (IxVector rng vec) i

-- | Span-based DP with preset index patterns.
spanDP :: (U.Unbox a) => Int -> a -> (Int -> a) -> (IxVector (Int, Int) (U.Vector a) -> (Int, Int) -> a) -> IxVector (Int, Int) (U.Vector a)
spanDP !n !undef !onOne !f = constructIV ((0, 0), (n + 1, n)) $ \vec (!spanLen, !spanL) ->
  if spanLen == 0 || spanL >= (n + 1 - spanLen)
    then undef
    else
      if spanLen == 1
        then onOne spanL
        else f vec (spanLen, spanL)

-- | Typical set-based DP.
--
-- = Typical problems
-- - [ABC 317 C - Remembering the Days](https://atcoder.jp/contests/abc317/tasks/abc317_c)
tspDP :: Int -> IxUVector (Int, Int) Int -> U.Vector Int
tspDP !nVerts !gr = U.constructN (nSets * nVerts) $ \vec -> case VG.length vec `divMod` nVerts of
  -- initial states
  (!s, !vTo) | s == bit vTo -> 0 :: Int
  -- non-reachable states
  (!s, !vTo) | not (testBit s vTo) -> undef
  -- possible transitions
  (!s, !vTo) ->
    let !s' = clearBit s vTo
        !candidates = (U.take nVerts . U.drop (nVerts * s')) vec
     in U.maximum $ flip U.imap candidates $ \vFrom w0 ->
          let !dw = gr @! (vFrom, vTo)
           in -- !_ = dbg (s, "<-", s', (vFrom, vTo), w0, dw, w0 + dw)
              bool (w0 + dw) undef (dw == undef || w0 == undef)
  where
    !nSets = bit nVerts
    !undef = -1 :: Int

-- | Enumerates all possible bitsets that composes @bit n - 1@.
--
-- >>> enumerateBitSets 4
-- [[8,4,2,1],[12,2,1],[8,6,1],[4,10,1],[14,1],[8,4,3],[12,3],[8,2,5],[10,5],[8,7],[4,2,9],[6,9],[4,11],[2,13],[15],[]]
--
-- = Typical problems
-- - [ABC 310 D - Peaceful Teams](https://atcoder.jp/contests/abc310/tasks/abc310_d)
-- - [ABC 319 D - General Weighted Max Matching](https://atcoder.jp/contests/abc318/tasks/abc318_d)
--   (Not the exact pattern though)
enumerateBitSets :: Int -> [[Int]]
enumerateBitSets !n = inner [[]] [] (bit n - 1)
  where
    inner :: [[Int]] -> [Int] -> Int -> [[Int]]
    inner !results !acc 0 = acc : results
    inner !results !acc !rest = U.foldl' step results (powersetVU rest')
      where
        !lsb = countTrailingZeros rest
        !rest' = clearBit rest lsb
        step !res !set =
          let !set' = set .|. bit lsb
           in inner res (set' : acc) (rest' .&. complement set')

-- | The input must be one-based
lcs :: HasCallStack => U.Vector Int -> Int
lcs !xs = runST $ do
  !stree <- newSTreeVU max (VG.length xs) (0 :: Int)

  U.forM_ xs $ \x -> do
    !n0 <- fromMaybe 0 <$> querySTree stree (0, x - 1)
    insertSTree stree x (n0 + 1)

  fromJust <$> querySTree stree (0, VG.length xs - 1)
