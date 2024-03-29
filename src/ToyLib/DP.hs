-- | Typical DP utilities
--
-- TODO: Refactor `relaxMany` variants.
module ToyLib.DP where

import Control.Monad (forM_)
import Control.Monad.ST
import Data.Bits
import Data.Bool (bool)
import qualified Data.ByteString.Char8 as BS
import Data.Ix
import Data.SegmentTree.Strict
import Data.Semigroup
import Data.Utils.Unindex
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Data.Vector.IxVector
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Stack (HasCallStack)
import Math.BitSet (powersetU)
import ToyLib.Debug
import ToyLib.Prelude (rangeU)

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
-- @relaxMany !f !vec0 !input !expander@ ~ @G.accumulate f vec0 $ G.concatMap expander input@
relaxMany :: (HasCallStack, G.Vector v a, G.Vector v (Int, a), G.Vector v b) => (a -> a -> a) -> v a -> v b -> (b -> v (Int, a)) -> v a
relaxMany !relax !vec0 !input !expander = G.create $ do
  !vec <- G.unsafeThaw vec0

  G.forM_ input $ \x -> do
    G.forM_ (expander x) $ \(!i, !x') -> do
      GM.modify vec (`relax` x') i

  return vec

-- | `relaxMany` with index input. Be wanrned that *the @input@ is consumed in-place*. Run like
-- @relaxMany vec0 (U.force vec0) $ \x -> ..@ if it needs to be cloned.
irelaxMany :: (HasCallStack, G.Vector v a, G.Vector v (Int, a), G.Vector v b) => (a -> a -> a) -> v a -> v b -> (Int -> b -> v (Int, a)) -> v a
irelaxMany !relax !vec0 !input !expander = G.create $ do
  !vec <- G.unsafeThaw vec0

  G.iforM_ input $ \ix x -> do
    G.forM_ (expander ix x) $ \(!i, !x') -> do
      GM.modify vec (`relax` x') i

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
pushBasedConstructN :: (HasCallStack, G.Vector v a, G.Vector v (Int, a)) => (a -> a -> a) -> v a -> (Int -> v a -> v (Int, a)) -> v a
pushBasedConstructN !relax !vec0 !expander = G.create $ do
  !vec <- G.unsafeThaw vec0

  forM_ [0 .. GM.length vec - 1] $ \iFrom -> do
    -- REMARK: Because this is a push-based DP, the value for the interested index is already known.
    !freezed <- G.unsafeFreeze (GM.take (iFrom + 1) vec)
    G.forM_ (expander iFrom freezed) $ \(!iTo, !x') -> do
      GM.modify vec (`relax` x') iTo

  return vec

pushBasedConstructIV :: (HasCallStack, Unindex i, U.Unbox a) => (a -> a -> a) -> IxUVector i a -> (IxUVector i a -> i -> U.Vector (i, a)) -> IxUVector i a
pushBasedConstructIV !relax !vec0 !expander = runST $ do
  !vec <- unsafeThawIV vec0
  let bnd@(!_, !_) = boundsIV vec0
  let !_ = dbgAssert (rangeSize bnd == GM.length (vecIV vec)) "wrong bounds"

  forM_ [0 .. GM.length (vecIV vec) - 1] $ \iFrom -> do
    let !iFrom' = unindex bnd iFrom
    -- Because this is a push-based DP, the value for the interested index is already known.
    !freezed <- unsafeFreezeIV vec
    U.forM_ (expander freezed iFrom') $ \(!iTo, !x') -> do
      modifyIV vec (`relax` x') iTo

  unsafeFreezeIV vec

-- | \([l, r]\) span.
type Span = (Int, Int)

-- | Returns non-empty splits of a `Span`.
-- >>> twoSplits 3 6
-- [((3,3),(4,6)),((3,4),(5,6)),((3,5),(6,6))]
{-# INLINE twoSplits #-}
twoSplits :: Int -> Int -> U.Vector (Span, Span)
twoSplits !l !r = U.map (\len -> ((l, l + len - 1), (l + len, r))) $ rangeU 1 (r - l)

-- | Returns two nullable splits of a span.
-- >>> iwiSpansU 3 6
-- [((3,2),(4,6)),((3,3),(5,6)),((3,4),(6,6)),((3,5),(7,6))]
{-# INLINE iwiSpansU #-}
iwiSpansU :: Int -> Int -> U.Vector (Span, Span)
iwiSpansU !l !r = U.map (\len -> ((l, l + len - 1), (l + len + 1, r))) $ rangeU 0 (r - l)

-- | Returns two nullables splits and a non-null mid point of a span.
-- >>> iwiSpansU' 3 6
-- [((3,2),3,(4,6)),((3,3),4,(5,6)),((3,4),5,(6,6)),((3,5),6,(7,6))]
{-# INLINE iwiSpansU' #-}
iwiSpansU' :: Int -> Int -> U.Vector (Span, Int, Span)
iwiSpansU' !l !r = U.map (\len -> ((l, l + len - 1), l + len, (l + len + 1, r))) $ rangeU 0 (r - l)

-- | Span-based DP with preset index patterns.
-- REMARK: @@sofar @! (l, r)@@
spanDP :: (U.Unbox a) => Int -> a -> (Int -> a) -> (IxVector (Int, Int) (U.Vector a) -> (Int, Int) -> a) -> IxVector (Int, Int) (U.Vector a)
spanDP !n !undef !onOne !f = constructIV ((0, 0), (n + 1, n)) $ \vec (!spanLen, !spanL) ->
  if spanLen == 0 || spanL >= (n + 1 - spanLen)
    then undef
    else
      if spanLen == 1
        then onOne spanL
        else -- f = fromKnown <> fromNew
          f vec (spanLen, spanL)

-- | Typical set-based DP (traveling salesman problem).
--
-- = Typical problems
-- - [ABC 317 C - Remembering the Days](https://atcoder.jp/contests/abc317/tasks/abc317_c)
tspDP :: Int -> IxUVector (Int, Int) Int -> U.Vector Int
tspDP !nVerts !gr = U.constructN (nSets * nVerts) $ \vec -> case G.length vec `divMod` nVerts of
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

-- | Powerset with the lsb on, mainly for partitioning DP.
ordPowerset :: Int -> U.Vector Int
ordPowerset 0 = U.empty
ordPowerset set0 = U.map (.|. lsb) . U.init $ powersetU set'
  where
    lsb = countTrailingZeros set0
    set' = clearBit set0 lsb

-- | Longest increasing subsequence. The input must be zero-based.
lisOf :: (HasCallStack) => U.Vector Int -> Int
lisOf !xs = runST $ do
  !stree <- buildSTree (U.replicate (G.length xs) (Max (0 :: Int)))

  U.forM_ xs $ \x -> do
    !n0 <- maybe 0 getMax <$> foldMaySTree stree 0 (x - 1)
    writeSTree stree x (Max (n0 + 1))

  getMax <$> foldAllSTree stree

-- | Longest common sequence.
lcsOf :: BS.ByteString -> BS.ByteString -> Int
lcsOf !s !t = U.last . vecIV . constructIV bnd $ \sofar i -> case i of
  (0, 0) -> 0 :: Int
  (0, _) -> 0 :: Int
  (_, 0) -> 0 :: Int
  (!ls, !lt) -> n1 `max` n2 `max` n3
    where
      n1 = sofar @! (ls - 1, lt)
      n2 = sofar @! (ls, lt - 1)
      n3
        | BS.index s (ls - 1) == BS.index t (lt - 1) = sofar @! (ls - 1, lt - 1) + 1
        | otherwise = 0
  where
    bnd = ((0, 0), (BS.length s, BS.length t))
