module Heuristic.Randoms where

import Control.Monad.State.Class (MonadState (state))
import Data.Vector.Unboxed qualified as U
import System.Random (RandomGen, UniformRange, uniformR)
import System.Random.Stateful
  ( RandomGen,
    StatefulGen,
    UniformRange (uniformRM),
    uniformR,
  )

-- | Generates `n` random values in `rng`.
--
-- >>> rolls 3 (0 :: Int , 100 :: Int) (mkStdGen 137)
-- [51,1,29]
rolls :: (RandomGen g, UniformRange a, U.Unbox a) => Int -> (a, a) -> g -> U.Vector a
rolls n rng = U.unfoldrExactN n (uniformR rng)

rollsM :: (StatefulGen g m, UniformRange a, U.Unbox a) => Int -> (a, a) -> g -> m (U.Vector a)
rollsM n rng = U.replicateM n . uniformRM rng

-- | `uniformR` where the `RandomGen` is given via `MonadState`.
uniformRSt :: (RandomGen g, UniformRange a, MonadState g m) => (a, a) -> m a
uniformRSt !rng = state (uniformR rng)

-- TODO: withTimeLimit
