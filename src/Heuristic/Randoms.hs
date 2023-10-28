module Heuristic.Randoms where

import System.Random
import System.Random.Stateful
import qualified Data.Vector.Unboxed as VU

-- | Generates `n` random values in `rng`.
--
-- >>> ghci> rolls 3 (0 :: Int , 100 :: Int) (mkStdGen 137)
-- [51,1,29]
rolls :: (RandomGen g, UniformRange a, VU.Unbox a) => Int -> (a, a) -> g -> VU.Vector a
rolls n rng = VU.unfoldrExactN n (uniformR rng)

rollsM :: (StatefulGen g m, UniformRange a, VU.Unbox a) => Int -> (a, a) -> g -> m (VU.Vector a)
rollsM n rng = VU.replicateM n . uniformRM rng

