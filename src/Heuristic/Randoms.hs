module Heuristic.Randoms where

import System.Random
import System.Random.Stateful
import qualified Data.Vector.Unboxed as U

-- | Generates `n` random values in `rng`.
--
-- >>> rolls 3 (0 :: Int , 100 :: Int) (mkStdGen 137)
-- [51,1,29]
rolls :: (RandomGen g, UniformRange a, U.Unbox a) => Int -> (a, a) -> g -> U.Vector a
rolls n rng = U.unfoldrExactN n (uniformR rng)

rollsM :: (StatefulGen g m, UniformRange a, U.Unbox a) => Int -> (a, a) -> g -> m (U.Vector a)
rollsM n rng = U.replicateM n . uniformRM rng

