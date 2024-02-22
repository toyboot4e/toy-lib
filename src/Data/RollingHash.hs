{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

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
import Data.ModInt
import Data.Proxy
import Data.Tuple.Extra hiding (first, second)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Exts
import GHC.TypeLits

-- TODO: Remove the use of Moint

-- | Rolling hash of a string.
-- = Cummulative sum
--
-- Slice (2, 4) of a string "abcde" is given as this:
--
-- >            s :=     a       b       c       d       e
-- >            s4 = b^4 a + b^3 b + b^2 c + b^1 d + b^0 e
-- >            s2 = b^1 a + b^0 b
-- > s4 - s2 * b^3 =                 b^2 c + b^1 d + b^0 e
--
-- TODO: Define `Group` class and generalize `csum1D`.
--
-- = Monoid
--
-- TODO
--
-- = Typical prolbems
--
-- [ABC 331 F - Palindrome Query](https://atcoder.jp/contests/abc331/tasks/abc331_f)
data RH b p = RH
  { lenRH :: !Int,
    -- | \$b^{lenRH - 1}$
    digitRH :: !(ModInt p),
    -- | The hash value.
    hashRH :: !(ModInt p)
  }
  -- TODO: ignore @digitRH@ on @Eq@
  deriving (Eq, Ord, Show)

-- | Creates a one-length `RH` from an integer.
--
-- = Warning
-- The input must be less than @p@.
{-# INLINE rh1 #-}
rh1 :: (KnownNat p) => Int -> RH b p
rh1 !x = RH 1 1 (ModInt x)

instance (KnownNat b, KnownNat p) => Semigroup (RH b p) where
  {-# INLINE (<>) #-}
  rh <> (RH 0 !_ !_) = rh
  (RH 0 !_ !_) <> rh = rh
  (RH !len1 !digit1 !hash1) <> (RH !len2 !digit2 !hash2) = RH (len1 + len2) digit' hash'
    where
      !b = fromInteger $ natVal' (proxy# @b) :: ModInt p
      !digit' = b * digit1 * digit2
      !hash' = hash1 * (b * digit2) + hash2

instance (KnownNat b, KnownNat p) => Monoid (RH b p) where
  {-# INLINE mempty #-}
  mempty = RH 0 1 0

-- | `RH` conversion type for unboxed vectors.
--
-- TODO: Unboxed implementation without the lazy tuples.
type RHRepr = (Int, Int, Int)

instance U.IsoUnbox (RH b p) RHRepr where
  {-# INLINE toURepr #-}
  toURepr (RH !a !b !c) = (a, unModInt b, unModInt c)
  {-# INLINE fromURepr #-}
  fromURepr (!a, !b, !c) = RH a (ModInt b) (ModInt c)

newtype instance U.MVector s (RH b p) = MV_RH (UM.MVector s RHRepr)

newtype instance U.Vector (RH b p) = V_RH (U.Vector RHRepr)

deriving via (RH b p `U.As` RHRepr) instance GM.MVector UM.MVector (RH b p)

deriving via (RH b p `U.As` RHRepr) instance G.Vector U.Vector (RH b p)

instance U.Unbox (RH b p)

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
