{-# LANGUAGE DataKinds #-}

-- | The rolling hash algorithm lets you create fastly (\(O(1)\)) comparable / concatanatable string
-- slice in after \(O(N)\) preparation.
--
-- I suspect if slices longer than the orignal string can be calculated without panic in my
-- implementation.
module Data.RollingHash where

import Control.Monad.State.Class
import Control.Monad.State.Strict (evalState)
import Data.Char (ord)
import Data.List (foldl')
import Data.Maybe
import Data.Proxy
import GHC.TypeLits
import Data.Tuple.Extra hiding (first, second)
import qualified Data.Vector.Unboxed as U

-- {{{ Rolling hash

-- TODO: rolling hash on a segment tree

-- | Rolling hash of a string.
--
-- = Internals
--
-- Slice (2, 4) of "abcdef" is given as this:
--
-- >            s :=     a       b       c       d       e
-- >            s4 = b^4 a + b^3 b + b^2 c + b^1 d + b^0 e
-- >            s2 = b^1 a + b^0 b
-- > s4 - s2 * b^3 =                 b^2 c + b^1 d + b^0 e
data RollingHash b p = RollingHash
  { sourceLength :: !Int,
    -- | \$\{B^i mod p\}_{i \elem [0, n)}$
    dimensions :: !(U.Vector Int),
    hashSum :: !(U.Vector Int)
  }
  deriving (Show, Eq)

-- | Type level integer that represents the B-adic number used for the rolling hash algorithm.
type HashInt = (100 :: Nat)

-- | Creates a rolling hash of given string.
newRH :: forall p. (KnownNat p) => String -> RollingHash HashInt p
newRH !source = RollingHash n bn hashSum_
  where
    !p = fromInteger $ natVal (Proxy @p) :: Int
    !b = fromInteger $ natVal (Proxy @HashInt) :: Int
    !n = length source
    !bn = U.iterateN (succ n) (\lastB -> b * lastB `mod` p) (1 :: Int)
    !hashSum_ = evalState (U.mapM (\ !ch -> state $ \ !acc -> f ch acc) $ U.fromList source) (0 :: Int)
      where
        f :: Char -> Int -> (Int, Int)
        f !ch !lastX = dupe $! (lastX * b + ord ch) `mod` p

-- | Retrieves the original length of the `RollingHash` string.
lengthRH :: RollingHash b p -> Int
lengthRH (RollingHash !len !_ !_) = len

-- | @HashSlice value length@. See also the example of `RollingHash`.
data HashSlice p = HashSlice
  { hashValue :: {-# UNPACK #-} !Int,
    -- hashOffset :: {-# UNPACK #-} !Int,
    hashLength :: {-# UNPACK #-} !Int
  }
  deriving (Show, Eq)

-- | Slices a rolling hash string.
sliceRH :: forall b p. (KnownNat p) => RollingHash b p -> Int -> Int -> HashSlice p
sliceRH (RollingHash !_ !bn !s) !i0 !i1
  -- TODO: add debug assertion
  | i0 > i1 = emptyHS
  | otherwise =
      let !len = i1 - i0 + 1
          !s1 = s U.! i1
          !s0 = fromMaybe 0 $ s U.!? pred i0
          !value = (s1 - (bn U.! len) * s0) `mod` p
       in HashSlice value len
  where
    !p = fromInteger $ natVal (Proxy @p) :: Int

-- | Cons two rolling hash slices.
consHS :: forall b p. (KnownNat p) => RollingHash b p -> HashSlice p -> HashSlice p -> HashSlice p
consHS (RollingHash !_ !bn !_) (HashSlice !v0 !l0) (HashSlice !v1 !l1) = HashSlice value len
  where
    !p = fromInteger $ natVal (Proxy @p) :: Int
    !value = ((bn U.! l1) * v0 + v1) `mod` p
    !len = l0 + l1

-- | Creates an empty rolling hash slice.
emptyHS :: HashSlice p
emptyHS = HashSlice 0 0

-- | Concatanates two rolling hash slices.
concatHS :: forall b p t. (KnownNat p, Foldable t) => RollingHash b p -> t (HashSlice p) -> HashSlice p
concatHS !rhash !slices = foldl' (consHS rhash) emptyHS slices

-- }}}
