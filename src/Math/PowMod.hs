-- | \(\mathit{base}^n \bmod x\) in a constant time using Fermet's little theorem.
--
-- TODO: Test all the functions.
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

-- | \(O(W)\) One-shot calculation of $\mathit{base} ^ \mathit{power} \bmod \mathit{modulo}$ in a
-- constant time.
{-# INLINE powModConst #-}
powModConst :: Int -> Int -> Int -> Int
powModConst !modulo !base !power = powModByCache (powModCache modulo (base `mod` modulo)) power

-- | \(O(W)\) One-shot calcaulation of \(x / d \bmod p\), using Fermat's little theorem.
{-# INLINE invModF #-}
invModF :: Int -> Int -> Int
invModF !modulo !d = invModFC (powModCache modulo d) modulo

-- | \(O(W)\) Calculates \(x / d \bmod p\), using Fermat's little theorem.
{-# INLINE divModF #-}
divModF :: Int -> Int -> Int -> Int
divModF !modulo !x !d = divModFC (powModCache modulo d) x `rem` modulo

-- | \(O(W)\) Cache of \(\mathit{base}^n\) for iterative square method.
powModCache :: Int -> Int -> (Int, U.Vector Int)
powModCache !modulo !base = (modulo, U.iterateN 63 (\x -> x * x `rem` modulo) base)

-- | \(O(W)\) Calculates \(\mathit{base}^n \bmod p\) using a cache.
{-# INLINE powModByCache #-}
powModByCache :: (Int, U.Vector Int) -> Int -> Int
powModByCache (!modulo, !cache) power = U.foldl' step 1 (bitsOf power)
  where
    step !acc nBit = acc * (cache U.! nBit) `rem` modulo

-- | \(O(W)\) \(1/d = d^{p-2} \bmod p\)
--
-- \(\because d^p = d \bmod p\) where the modulo is a prime number and @d@ is not a mulitple of
-- @p@.
{-# INLINE invModFC #-}
invModFC :: (Int, U.Vector Int) -> Int -> Int
invModFC context primeModulo = powModByCache context (primeModulo - 2)

-- | \(O(W)\) \(1/d = d^{p-2} \bmod p\)
{-# INLINE divModFC #-}
divModFC :: (Int, U.Vector Int) -> Int -> Int
divModFC context@(!modulo, !_) x = x * invModFC context modulo `rem` modulo

-- | \(O(N)\) Cache of \(n! \bmod m\) up to `n`.
{-# INLINE factModsN #-}
factModsN :: Int -> Int -> U.Vector Int
factModsN !modulo !n = U.scanl' (mulMod modulo) (1 :: Int) $ U.generate n (+ 1)

-- | \(O(N)\) nCr `mod` m (binominal cofficient).
{-# INLINE bcMod #-}
bcMod :: Int -> Int -> Int -> Int
bcMod !n !r !modulo = foldl' (divModF modulo) (facts U.! n) [facts U.! r, facts U.! (n - r)]
  where
    facts = factModsN modulo n
