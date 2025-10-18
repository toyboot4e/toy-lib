-- | The number of divisor is smaller than \(log N\).
-- See also: <https://scrapbox.io/magurofly/%E7%B4%84%E6%95%B0%E3%81%AE%E5%80%8B%E6%95%B0%E3%81%AE%E3%82%AA%E3%83%BC%E3%83%80%E3%83%BC>
--
-- = Tips
-- - The last number could be decided with division, not enumeration.
--
-- = Typical problems
-- - [Typical 085 - Multiplication (★4)](https://atcoder.jp/contests/typical90/tasks/typical90_cg)
-- - [ABC 300 D - AABBC (Difficulty 908)](https://atcoder.jp/contests/abc300/tasks/abc300_d)
--
-- = Prime factors
-- - [Typical 030 - K Factors (★5)](https://atcoder.jp/contests/typical90/tasks/typical90_ad)
--   Numbers with i >= k prime factors can be enumerated very fastly.
--
-- = Split and list / meet in the middle
-- - [Typical 051 - Typical Shop (★5)](https://atcoder.jp/contests/typical90/tasks/typical90_ay)
--
-- = Other problems
-- - [ABC 254 D - Together Square (Difficulty 1191)](https://atcoder.jp/contests/abc254/tasks/abc254_d)
module Math.Divisors where

import qualified AtCoder.Extra.Math as EM
import Control.Monad.Primitive
import Data.Foldable (for_)
import Data.List
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- \(O(N \log N)\) Divisor enumeration (sorted). Too slow, never use it.
divisorsOf :: Int -> [Int]
divisorsOf n = sort $ inner 1
  where
    inner k
      -- no dependency to `Int` square root function
      | k * k > n = []
      -- no divisor duplication
      | k * k == n = [k]
      -- not sorted yet
      | r == 0 = k : d : inner (succ k)
      -- ignore non divisors
      | otherwise = inner (succ k)
      where
        -- This strict evaluation and unboxing takes some effect, even though they're not always
        -- used.
        (!d, !r) = n `divMod` k

-- | For instance, [6], i.e., [0, 0, 0, 0, 0, 0, 1] becomes [0, 1, 1, 1, 0, 0, 1].
countDivisorsInPlace :: (PrimMonad m, GM.MVector v a, Num a) => U.Vector Int -> v (PrimState m) a -> m ()
countDivisorsInPlace ps cnt = do
  let maxX = GM.length cnt - 1
  -- For example, for 6, we get [0, 1, 1, 1, 0, 0, 1] from [0, 0, 0, 0, 0, 0, 1] with the following steps:
  -- - p = 2: cnt[3] += cnt[6], cnt[2] += cnt[4], cnt[1] += cnt[2]
  -- - p = 3: cnt[2] += cnt[6]
  U.forM_ ps $ \p -> do
    let np = maxX `div` p
    for_ [np, np - 1 .. 1] $ \i -> do
      GM.modifyM
        cnt
        ( \acc -> do
            dx <- GM.read cnt (i * p)
            pure $! acc + dx
        )
        i

-- | For instance, [6], i.e., [0, 0, 0, 0, 0, 0, 1] becomes [0, 1, 1, 1, 0, 0, 1].
--
-- ==== Typical problems
-- - [ABC 393 - E: GCD of Subset](https://atcoder.jp/contests/abc393/tasks/abc393_e)
countDivisors :: U.Vector Int -> U.Vector Int
countDivisors xs = U.create $ do
  let maxX = U.maximum xs
  cnt <- UM.replicate (maxX + 1) (0 :: Int)
  U.forM_ xs $ \x -> do
    GM.modify cnt (+ 1) x
  let ps = EM.primes maxX
  countDivisorsInPlace ps cnt
  pure cnt
