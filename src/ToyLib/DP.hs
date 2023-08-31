-- | Typical DP utilities
module ToyLib.DP where

import Data.Bits
import Data.Bool (bool)
import Data.Ix
import Data.Unindex
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import Data.Vector.IxVector
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import ToyLib.Prelude (rangeVU)

-- | Variant of `VU.constructN`.
constructFor :: (VU.Unbox a, VU.Unbox b) => a -> VU.Vector b -> (VU.Vector a -> b -> a) -> VU.Vector a
constructFor !x0 !input !f = VU.create $ do
  !vec <- VUM.unsafeNew (VU.length input + 1)
  VUM.unsafeWrite vec 0 x0

  flip VU.imapM_ input $ \lenS1 x -> do
    !vec' <- VU.take (succ lenS1) <$> VU.unsafeFreeze vec
    VUM.unsafeWrite vec (succ lenS1) $! f vec' x

  return vec

-- | @relaxMany !f !vec0 !input !expander@ ~ @VG.accumulate f vec0 $ VG.concatMap expander input@
relaxMany :: (VG.Vector v a, VG.Vector v (Int, a), VG.Vector v b) => (a -> a -> a) -> v a -> v b -> (b -> v (Int, a)) -> v a
relaxMany !relax !vec0 !input !expander = VG.create $ do
  !vec <- VG.unsafeThaw vec0

  VG.forM_ input $ \x -> do
    VG.forM_ (expander x) $ \(!i, !x') -> do
      VGM.modify vec (`relax` x') i

  return vec

-- | Monoid variant of `relaxMany`
relaxMany' :: (Monoid m, VU.Unbox m, VU.Unbox a) => VU.Vector m -> VU.Vector a -> (a -> VU.Vector (Int, m)) -> VU.Vector m
relaxMany' !vec0 !input !expander = VU.create $ do
  !vec <- VU.unsafeThaw vec0

  VU.forM_ input $ \x -> do
    VU.forM_ (expander x) $ \(!i, !x') -> do
      VUM.modify vec (<> x') i

  return vec

-- | Returns non-zero two spans over the given inclusive range @[l, r]@.
-- >>> spansVU 3 6
-- [((3,3),(4,6)),((3,4),(5,6)),((3,5),(6,6))]
spansVU :: Int -> Int -> VU.Vector ((Int, Int), (Int, Int))
spansVU !l !r = VU.map (\len -> ((l, l + len - 1), (l + len, r))) $ rangeVU 1 (r - l)

-- | `VU.constructN` for `IxVector`
constructIV :: (Unindex i, VU.Unbox a) => (i, i) -> (IxVector i (VU.Vector a) -> i -> a) -> IxVector i (VU.Vector a)
constructIV !rng !f = IxVector rng $ VG.constructN (rangeSize rng) $ \vec ->
  let !i = unindex rng (VG.length vec)
   in f (IxVector rng vec) i

-- | Span-based DP with preset index patterns.
spanDP :: (VU.Unbox a) => Int -> a -> (Int -> a) -> (IxVector (Int, Int) (VU.Vector a) -> (Int, Int) -> a) -> IxVector (Int, Int) (VU.Vector a)
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
tspDP :: Int -> IxUVector (Int, Int) Int -> VU.Vector Int
tspDP !nVerts !gr = VU.constructN (nSets * nVerts) $ \vec -> case VG.length vec `divMod` nVerts of
  -- initial states
  (!s, !vTo) | s == bit vTo -> 0 :: Int
  -- non-reachable states
  (!s, !vTo) | not (testBit s vTo) -> undef
  -- possible transitions
  (!s, !vTo) ->
    let !s' = clearBit s vTo
        !candidates = (VU.take nVerts . VU.drop (nVerts * s')) vec
     in VU.maximum $ flip VU.imap candidates $ \vFrom w0 ->
          let !dw = gr @! (vFrom, vTo)
           in -- !_ = dbg (s, "<-", s', (vFrom, vTo), w0, dw, w0 + dw)
              bool (w0 + dw) undef (dw == undef || w0 == undef)
  where
    !nSets = bit nVerts
    !undef = -1 :: Int
