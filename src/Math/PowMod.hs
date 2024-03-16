-- | \(\mathit{base}^n \bmod x\) in a constant time using Fermet's little theorem.
module Math.PowMod where

import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Math.BitSet (bitsOf)

addMod, subMod, mulMod :: Int -> Int -> Int -> Int
{-# INLINE addMod #-}
addMod !modulo !x !a = (x + a) `mod` modulo
{-# INLINE subMod #-}
subMod !modulo !x !s = (x - s) `mod` modulo
{-# INLINE mulMod #-}
mulMod !modulo !b !p = (b * p) `mod` modulo

-- | \(n! \bmod m\) stupid calculation.
factMod :: Int -> Int -> Int
factMod 0 _ = 1
factMod 1 _ = 1
factMod !n !m = n * factMod (n - 1) m `rem` m

-- F: Fermet, FC: Fermet by cache

-- | One-shot calculation of $\mathit{base} ^ \mathit{power} \bmod \mathit{modulo}$ in a constant
-- time.
{-# INLINE powModConst #-}
powModConst :: Int -> Int -> Int -> Int
powModConst !base !power !modulo = powModByCache power (powModCache (base `mod` modulo) modulo)

-- | One-shot calcaulation of \(x / d \bmod p\), using Fermat's little theorem.
{-# INLINE invModF #-}
invModF :: Int -> Int -> Int
invModF !d !modulo = invModFC modulo (powModCache d modulo)

-- | Calculates \(x / d \bmod p\), using Fermat's little theorem.
{-# INLINE divModF #-}
divModF :: Int -> Int -> Int -> Int
divModF !x !d !modulo = divModFC x (powModCache d modulo) `rem` modulo

-- | Cache of \(\mathit{base}^n\) for iterative square method.
powModCache :: Int -> Int -> (Int, U.Vector Int)
powModCache !base !modulo = (modulo, bl)
  where
    bl = U.iterateN 63 (\x -> x * x `rem` modulo) base

-- | Calculates \(\mathit{base}^n \bmod p\) using a cache.
{-# INLINE powModByCache #-}
powModByCache :: Int -> (Int, U.Vector Int) -> Int
powModByCache !power (!modulo, !cache) = U.foldl' step 1 (bitsOf power)
  where
    step !acc !nBit = acc * (cache U.! nBit) `rem` modulo

-- | \(1/d = d^{p-2} \bmod p\)
--
-- \(\because d^p = d \bmod p\) where the modulo is a prime number and @d@ is not a mulitple of
-- @p@. and \(x^{p-2}\) is pre-calculated with cache.
{-# INLINE invModFC #-}
invModFC :: Int -> (Int, U.Vector Int) -> Int
invModFC !primeModulo = powModByCache (primeModulo - 2)

{-# INLINE divModFC #-}
divModFC :: Int -> (Int, U.Vector Int) -> Int
divModFC !x context@(!modulo, !_) = x * invModFC modulo context `rem` modulo

-- | Cache of \(n! \bmod m\) up to `n`.
{-# INLINE factModsN #-}
factModsN :: Int -> Int -> U.Vector Int
factModsN !n !modulo =
  U.scanl' (\ !x !y -> x * y `rem` modulo) (1 :: Int) $ U.fromList [(1 :: Int) .. n]

-- | nCr `mod` m (binominal cofficient).
{-# INLINE bcMod #-}
bcMod :: Int -> Int -> Int -> Int
bcMod !n !r !modulo = foldl' (\ !x !y -> divModF x y modulo) (facts U.! n) [facts U.! r, facts U.! (n - r)]
  where
    facts = factModsN n modulo
