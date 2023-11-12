-- | Mo's algorithm
--
-- REMARK: Be sure to @{-# INLINE #-}@ the user-defined functions!
--
-- = Typical problems
-- - [ABC 293 G - Triple Index](https://atcoder.jp/contests/abc293/tasks/abc293_g)
module Algorithm.Mo where

import Control.Monad.Primitive (PrimMonad)
import qualified Data.Vector.Algorithms.Intro as VAI
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import ToyLib.Prelude (rangeU)

-- | Returns a sorted indices to the vector of @(l, r)@, first by @l `div` b@ and then @r@.
{-# INLINE sortMo #-}
sortMo :: Int -> U.Vector (Int, Int) -> U.Vector Int
sortMo !maxL !lrs = U.modify (VAI.sortBy compareF) (U.generate q id)
  where
    !q = G.length lrs
    -- TODO: consider length of @maxL / sqrt Q@.
    !blockLength = max 1 $ (ceiling @Double . sqrt . fromIntegral) maxL
    -- compare by block, then compare by right
    compareF !i1 !i2 =
      let (!l1, !r1) = lrs U.! i1
          !b1 = l1 `div` blockLength
          (!l2, !r2) = lrs U.! i2
          !b2 = l2 `div` blockLength
       in compare b1 b2 <> compare r1 r2

-- | Runs Mo's algorithm: /O(N * \sqrt Q).
{-# INLINE runMo #-}
runMo :: (PrimMonad m, U.Unbox x, U.Unbox a) => U.Vector x -> U.Vector (Int, Int) -> (a -> x -> m a) -> (a -> x -> m a) -> (a -> x -> m a) -> (a -> x -> m a) -> a -> m (U.Vector a)
runMo !xs !lrs !onInsL !onInsR !onRemL !onRemR !s0 = do
  !result <- UM.unsafeNew q
  U.foldM'_ (step result) ((0 :: Int, -1 :: Int), s0) (sortMo maxL lrs)
  U.unsafeFreeze result
  where
    !q = G.length lrs
    !maxL = U.maximum (U.map fst lrs)

    step result ((!l0, !r0), !n0) iLr = do
      let (!l, !r) = lrs U.! iLr
      !n' <- do
        !n1 <- U.foldM' onInsL n0 (U.backpermute xs (rangeU l (l0 - 1)))
        !n2 <- U.foldM' onInsR n1 (U.backpermute xs (rangeU (r0 + 1) r))
        !n3 <- U.foldM' onRemL n2 (U.backpermute xs (rangeU l0 (l - 1)))
        !n4 <- U.foldM' onRemR n3 (U.backpermute xs (rangeU (r + 1) r0))
        return n4

      UM.write result iLr n'
      return ((l, r), n')

