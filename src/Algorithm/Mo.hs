-- | \(O(N \sqrt Q)\) Mo's algorithm
--
-- Be sure to..
--
-- - @{-# INLINE #-}@ user-defined functions
-- - Never use unboxed vector for output
--
-- = Typical problems
-- - [ABC 293 G - Triple Index](https://atcoder.jp/contests/abc293/tasks/abc293_g)
module Algorithm.Mo where

import Control.Monad.Primitive (PrimMonad)
import Control.Monad.ST (runST)
import qualified Data.Vector.Algorithms.Intro as VAI
import Data.Bool (bool)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import ToyLib.Prelude (slice, (.:))

-- | \(O(N \log N)\) Returns a sorted indices to the vector of @(l, r)@, first by @l `div` b@ and
-- then @r@.
{-# INLINE sortMo #-}
sortMo :: Int -> U.Vector (Int, Int) -> U.Vector Int
sortMo !maxL !lrs = U.modify (VAI.sortBy compareF) (G.generate (G.length lrs) id)
  where
    -- !blockLength = max 1 $ (ceiling @Double . sqrt . fromIntegral) maxL
    !q = G.length lrs
    !blockLength = max 1 . ceiling @Double $ (fromIntegral maxL / sqrt (fromIntegral q))
    -- compare by block, then compare by right
    compareF !i1 !i2 =
      let (!l1, !r1) = lrs G.! i1
          (!l2, !r2) = lrs G.! i2
          !b1 = l1 `div` blockLength
          !b2 = l2 `div` blockLength
          !res = compare b1 b2 <> bool (compare r2 r1) (compare r1 r2) (even b1)
       in res

-- | \(O(N (\log N + \sqrt Q))\). Mo's algorithm: @runMo xs lrs onInsL onInsR onRemL onRemR extract state0@.
runMoG :: (PrimMonad m, U.Unbox x, G.Vector v b) => U.Vector x -> U.Vector (Int, Int) -> (a -> x -> m a) -> (a -> x -> m a) -> (a -> x -> m a) -> (a -> x -> m a) -> (a -> b) -> a -> m (v b)
runMoG !xs !lrs !onInsL !onInsR !onRemL !onRemR !extract !state0 = do
  !result <- GM.unsafeNew q
  U.foldM'_ (step result) ((0 :: Int, -1 :: Int), state0) (sortMo maxL lrs)
  G.unsafeFreeze result
  where
    !q = G.length lrs
    !maxL = U.maximum (U.map fst lrs)

    step result ((!l0, !r0), !s0) iLrs = do
      let (!l, !r) = lrs G.! iLrs
      !s' <- do
        !s1 <- U.foldM' onInsL s0 (slice l (l0 - 1) xs)
        !s2 <- U.foldM' onInsR s1 (slice (r0 + 1) r xs)
        !s3 <- U.foldM' onRemL s2 (slice l0 (l - 1) xs)
        !s4 <- U.foldM' onRemR s3 (slice (r + 1) r0 xs)
        pure s4

      GM.unsafeWrite result iLrs $! extract s'
      pure ((l, r), s')

-- | \(O(N (\log N + \sqrt Q))\). Pure variant of @runMoG@.
runMoPureG :: (U.Unbox x, G.Vector v b) => U.Vector x -> U.Vector (Int, Int) -> (a -> x -> a) -> (a -> x -> a) -> (a -> x -> a) -> (a -> x -> a) -> (a -> b) -> a -> v b
runMoPureG !xs !lrs !onInsL !onInsR !onRemL !onRemR !extract !state0 = runST $ do
  runMoG xs lrs (pure .: onInsL) (pure .: onInsR) (pure .: onRemL) (pure .: onRemR) extract state0

-- | \(O(N (\log N + \sqrt Q))\). Type-restricted `runMo`
runMo :: (PrimMonad m, U.Unbox x, U.Unbox b) => U.Vector x -> U.Vector (Int, Int) -> (a -> x -> m a) -> (a -> x -> m a) -> (a -> x -> m a) -> (a -> x -> m a) -> (a -> b) -> a -> m (U.Vector b)
runMo = runMoG

-- | \(O(N (\log N + \sqrt Q))\). Type-restricted `runMoPure`
runMoPure :: (U.Unbox x, U.Unbox b) => U.Vector x -> U.Vector (Int, Int) -> (a -> x -> a) -> (a -> x -> a) -> (a -> x -> a) -> (a -> x -> a) -> (a -> b) -> a -> U.Vector b
runMoPure = runMoPureG

-- | \(O(N (\log N + \sqrt Q))\). Run Mo's algorithm simply
simpleRunMo :: (PrimMonad m, U.Unbox x, U.Unbox a) => U.Vector x -> U.Vector (Int, Int) -> (a -> x -> m a) -> (a -> x -> m a) -> a -> m (U.Vector a)
simpleRunMo !xs !lrs !onIns !onRem !state0 = runMo xs lrs onIns onIns onRem onRem id state0

-- | \(O(N (\log N + \sqrt Q))\). Run Mo's algorithm simply
simpleRunMoPure :: (U.Unbox x, U.Unbox a) => U.Vector x -> U.Vector (Int, Int) -> (a -> x -> a) -> (a -> x -> a) -> a -> U.Vector a
simpleRunMoPure !xs !lrs !onIns !onRem !state0 = runMoPure xs lrs onIns onIns onRem onRem id state0

-- -- | Hilbert order
-- hilbertValue :: Int -> Int -> Int -> Int
-- hilbertValue !n2 = inner (n2 - 1) 0
--   where
--     inner (-1) !acc !_ !_ = acc
--     inner !s !acc !x !y
--       | ry == 1 = inner s' acc' x y
--       | rx == 1 = inner s' acc' (complement y) (complement x)
--       | otherwise = inner s' acc' y x
--       where
--         !s' = s - 1
--         !rx = bool 0 1 $ testBit x s
--         !ry = bool 0 1 $ testBit y s
--         !acc' = (shiftL acc 2) .|. (shiftL rx 1 `xor` ry)

-- -- <https://atcoder.jp/contests/past202212-open/submissions/40185399>
-- constexpr unsigned long hilbert_order(unsigned long x, unsigned long y){
-- 	unsigned long d{}, s{20};
-- 	for(; s--;){
-- 		unsigned long rx{1UL & (x >> s)}, ry{1UL & (y >> s)};
-- 		(d <<= 2) |= rx * 3 ^ ry;
-- 		if (!ry) {
-- 			if (rx) {
-- 				x = ~x;
-- 				y = ~y;
-- 			}
-- 			std::swap(x, y);
-- 		}
-- 	}
-- 	pure d;
-- }

