-- | \(\mathit{base}^n \bmod x\) in a constant time using Fermet's little theorem.
--
-- TODO: Test all the functions.
module Math.PowMod where

import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as G
import Math.Stimes (mulTimes)

addMod, subMod, mulMod :: Int -> Int -> Int -> Int
{-# INLINE addMod #-}
addMod !modulo !x !a = (x + a) `mod` modulo
{-# INLINE subMod #-}
subMod !modulo !x !s = (x - s) `mod` modulo
{-# INLINE mulMod #-}
mulMod !modulo !b !p = (b * p) `mod` modulo

-- | \(O(N)\) Naive calculation.
factMod :: Int -> Int -> Int
factMod 0 _ = 1
factMod 1 _ = 1
factMod !n !m = n * factMod (n - 1) m `rem` m

-- | \(O(W)\) \(\mathit{base} ^ \mathit{power} \bmod \mathit{modulo}\) using binary lifting.
{-# INLINE powModConst #-}
powModConst :: Int -> Int -> Int -> Int
powModConst !modulo !base !power = mulTimes power (mulMod modulo) base

-- | \(O(W)\) \(x / d \bmod p\) using Fermat's little theorem and binary lifting.
{-# INLINE invModConst #-}
invModConst :: Int -> Int -> Int
invModConst !primeModulo !d = powModConst primeModulo d (primeModulo - 2)

-- | \(O(W)\) \(x / d \bmod p\) using Fermat's little theorem and binary lifting.
{-# INLINE divModConst #-}
divModConst :: Int -> Int -> Int -> Int
divModConst !primeModulo !x !d = mulMod primeModulo x (invModConst primeModulo d)

-- | \(O(N)\) Cache of \(n! \bmod m\) up to `n`.
{-# INLINE factModsN #-}
factModsN :: Int -> Int -> U.Vector Int
factModsN !modulo !n = U.scanl' (mulMod modulo) (1 :: Int) $ U.generate n (+ 1)

-- | \(O(N)\) nCr `mod` m (binominal cofficient).
{-# INLINE bcMod #-}
bcMod :: Int -> Int -> Int -> Int
bcMod !n !r !modulo = foldl' (divModConst modulo) (facts G.! n) [facts G.! r, facts G.! (n - r)]
  where
    facts = factModsN modulo n
