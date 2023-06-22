{-# LANGUAGE BangPatterns #-}

-- | TODO, Combine ModInt as Modulo module.

module Math.PowMod where

import Data.BinaryLifting
import Data.Bits
import Data.List (foldl')
import qualified Data.Vector.Unboxed as VU

-- {{{ Modulo arithmetic

-- TODO: refactor
-- TODO: consider taking `modulus` as the first argument

addMod, subMod, mulMod :: Int -> Int -> Int -> Int
addMod !x !a !modulus = (x + a) `mod` modulus
subMod !x !s !modulus = (x - s) `mod` modulus
mulMod !b !p !modulus = (b * p) `mod` modulus

-- | n! `mod` m
factMod :: Int -> Int -> Int
factMod 0 _ = 1
factMod 1 _ = 1
factMod !n !m = n * factMod (n - 1) m `rem` m

-- F: Fermet, FC: Fermet by cache

-- | One-shot calculation of $base ^ power `mod` modulo$ in a constant time
powModConst :: Int -> Int -> Int -> Int
powModConst !base !power !modulo = powByCache power (powModCache base modulo)

-- | One-shot calcaulation of \(x / d mod p\), using Fermat's little theorem.
--
-- \(1/d = d^{p-2} (\mod p) \equiv d^p = d (\mod p)\)
--   where the modulus is a prime number and `x` is not a mulitple of `p`.
invModF :: Int -> Int -> Int
invModF !d !modulus = invModFC modulus (powModCache d modulus)

-- | Calculates \(x / d mod p\), using Fermat's little theorem.
divModF :: Int -> Int -> Int -> Int
divModF !x !d !modulus = divModFC x (powModCache d modulus) `rem` modulus

-- | Cache of \(base^i\) for iterative square method.
powModCache :: Int -> Int -> (Int, VU.Vector Int)
powModCache !base !modulo = (modulo, doubling)
  where
    -- doubling = VU.scanl' (\ !x _ -> x * x `rem` modulo) base $ rangeVG (1 :: Int) 62
    doubling = newDoubling base (\x -> x * x `rem` modulo)

-- | Calculates \(base^i\) (mod p) from a cache.
powByCache :: Int -> (Int, VU.Vector Int) -> Int
-- TODO: test if it works as expeted
-- powByCache !power (!modulo, !cache) = applyDoubling cache 1 (\acc x -> acc * x `rem` modulo) power
powByCache !power (!modulo, !cache) = foldl' step 1 [0 .. 62]
  where
    step !acc !nBit =
      if testBit power nBit
        then acc * (cache VU.! nBit) `rem` modulo
        else acc

-- \(1/d = d^{p-2} (\mod p) \equiv d^p = d (\mod p)\)
--   where the modulus is a prime number and `x` is not a mulitple of `p`.
--
-- and \(x^{p-2}\) is calculated with cache.
invModFC :: Int -> (Int, VU.Vector Int) -> Int
invModFC !primeModulus = powByCache (primeModulus - 2)

divModFC :: Int -> (Int, VU.Vector Int) -> Int
divModFC !x context@(!modulus, !_) = x * invModFC modulus context `rem` modulus

-- | Cache of \(n! \mod m\) up to `n`.
factMods :: Int -> Int -> VU.Vector Int
factMods !n !modulus =
  VU.scanl' (\ !x !y -> x * y `rem` modulus) (1 :: Int) $ VU.fromList [(1 :: Int) .. n]

-- | nCr `mod` m (binominal cofficient).
bcMod :: Int -> Int -> Int -> Int
bcMod !n !r !modulus = foldl' (\ !x !y -> divModF x y modulus) (facts VU.! n) [facts VU.! r, facts VU.! (n - r)]
  where
    facts = factMods n modulus

-- }}}
