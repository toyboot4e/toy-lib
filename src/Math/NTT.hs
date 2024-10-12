{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}

-- | Number-theoretic transform (integer DFT).
module Math.NTT where

import Control.Monad (forM_)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST
import Data.Bits
import Data.Coerce
import Data.Maybe (fromJust)
import Data.ModInt
import Data.Ord (comparing)
import qualified Data.Vector.Algorithms.Intro as VAI
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Exts (proxy#)
import GHC.TypeLits
import Math.BitSet (ceil2)
import Math.Exgcd
import Math.PowMod (powModConst)
import ToyLib.Debug

bitRevSort :: (G.Vector v a, G.Vector v (Int, a)) => v a -> v a
bitRevSort =
  G.map snd
    . G.modify (VAI.sortBy (comparing (bitReverse . fst)))
    . G.indexed

-- | \(\Theta(N \log N)\) Number-theoretic transform (integer DFT).
--
-- >>> :set -XDataKinds
-- >>> ntt $ U.fromList $ map (ModInt @998244353) [1, 1, 1, 1]
-- [4,0,0,0]
-- >>> ntt $ U.fromList $ map (ModInt @469762049) [123, 0, 0, 0]
-- [123,123,123,123]
ntt :: (KnownNat p) => U.Vector (ModInt p) -> U.Vector (ModInt p)
ntt xs =
  U.modify butterfly
    . bitRevSort
    $ grow2 xs

-- | \(\Theta(N \log N)\) Inverse number-theoretic transform (integer DFT).
intt :: (KnownNat p) => U.Vector (ModInt p) -> U.Vector (ModInt p)
intt xs =
  bitRevSort
    . U.map (* invN)
    . U.modify invButterfly
    $ grow2 xs
  where
    !invN = recip (ModInt (U.length xs))

-- | \(\Theta((N + M) \log (N + M))\) Convolution with mod.
--
-- \(\mathcal{F}[f*g](\omega) = \mathcal{F}[f](\omega) \mathcal{F}[g](\omega)\).
--
-- \(\mathbb{a} * \mathbb{b} = \mathcal{F^{-1}}(\mathcal{F}(\mathbb{a}) \mathcal{F}(\mathbb{b}))\)
--
-- >>> :set -XDataKinds
-- >>> convoluteMod @998244353 (U.fromList [1, 1, 1, 0]) (U.fromList [1, 1, 1, 0])
-- [1,2,3,2,1,0,0]
-- >>> convoluteMod @998244353 (U.fromList [1, 1, 1]) (U.fromList [1, 1, 1, 0])
-- [1,2,3,2,1,0]
convoluteMod :: forall p. (KnownNat p) => U.Vector (ModInt p) -> U.Vector (ModInt p) -> U.Vector (ModInt p)
convoluteMod xs1 xs2
  | U.null xs1 || U.null xs2 = U.empty
convoluteMod xs1 xs2 = runST $ do
  -- F(a)
  vec1 <- U.unsafeThaw $ bitRevSort $ xs1 U.++ U.replicate (len - len1) (ModInt 0)
  butterfly vec1

  -- F(b)
  vec2 <- U.unsafeThaw $ bitRevSort $ xs2 U.++ U.replicate (len - len2) (ModInt 0)
  butterfly vec2

  -- F(a) F(b)
  vec2' <- U.unsafeFreeze vec2
  U.iforM_ vec2' $ \i y -> do
    UM.modify vec1 (* y) i

  -- F^{-1}(F(a) F(b)):
  invButterfly vec1

  let !invLen = recip $ ModInt len
  U.map (* invLen) . U.take (len1 + len2 - 1) . bitRevSort <$> U.unsafeFreeze vec1
  where
    !len1 = U.length xs1
    !len2 = U.length xs2
    !len = ceil2 (len1 + len2 - 1)

-- | Modulo type variable for CRT in `convolute64`.
type M1 = (754974721 :: Nat)

-- | Modulo type variable for CRT in `convolute64`.
type M2 = (167772161 :: Nat)

-- | Modulo type variable for CRT in `convolute64`.
type M3 = (469762049 :: Nat)

-- | \(\Theta((N+M)\log(N+M))\) Convolution without mod (much heavier than `convoluteMod`).
--
-- See also: [convolution_ll (ACL)](https://github.com/atcoder/ac-library/blob/master/atcoder/convolution.hpp).
convolute64 :: U.Vector Int -> U.Vector Int -> U.Vector Int
convolute64 !xs1 !xs2 =
  let !c1 = U.map coerce $ convoluteMod @M1 (U.map coerce xs1) (U.map coerce xs2)
      !c2 = U.map coerce $ convoluteMod @M2 (U.map coerce xs1) (U.map coerce xs2)
      !c3 = U.map coerce $ convoluteMod @M3 (U.map coerce xs1) (U.map coerce xs2)
   in (\f -> U.zipWith3 f c1 c2 c3) $ \x1 x2 x3 ->
        let !x =
              (x1 * i1) `mod` m1 * m2m3
                + (x2 * i2) `mod` m2 * m3m1
                + (x3 * i3) `mod` m3 * m1m2
            -- TODO: safeMod??
            !diff = x1 - (x `mod` m1)
            !diff' = if diff < 0 then diff + m1 else diff
         in x - offsets G.! (diff' `mod` 5)
  where
    !m1 = 754974721 :: Int -- 2^24
    !m2 = 167772161 :: Int -- 2^25
    !m3 = 469762049 :: Int -- 2^26
    !m2m3 = m2 * m3
    !m3m1 = m3 * m1
    !m1m2 = m1 * m2
    !m1m2m3 = m1 * m2 * m3
    !i1 = fromJust $ invModGcd m2m3 m1
    !i2 = fromJust $ invModGcd m3m1 m2
    !i3 = fromJust $ invModGcd m1m2 m3
    !offsets = U.fromListN 5 [0, 0, m1m2m3, 2 * m1m2m3, 3 * m1m2m3]

-- TODO: bang or not

-- | \(\Theta(N \log N)\)
butterfly :: forall p m. (KnownNat p, PrimMonad m) => UM.MVector (PrimState m) (ModInt p) -> m ()
butterfly xs = do
  -- g^{p-1/2^m} = 1 \pmod p
  let !p = fromInteger (natVal' (proxy# @p)) :: Int
  let !minRot = ModInt @p $ powModConst p (primRoot p) ((p - 1) .>>. iMaxBit)
  let !rots = U.map recip $ U.iterateN iMaxBit (\x -> x * x) minRot
  U.iforM_ (U.reverse rots) $ \iBit0 rotN1 -> do
    butterfly1 xs rotN1 iMaxBit (iBit0 + 1)
  where
    -- TODO: validate the number of digits
    !_ = dbgAssert (popCount (UM.length xs) == 1) "not a power of two"
    !iMaxBit = countTrailingZeros $ UM.length xs

-- | \(\Theta(N)\) Butterfly calculation performed in-place.
--
-- - @iMaxBit@: Length of @xs@ equals to @bit iMaxBit@
-- - @iBit@: The length of a pair equals to the length of @xs@.
--
-- See also: [my devlog post](http://toyboot4e.github.io/diary/2024-05-12.html#%E3%83%90%E3%82%BF%E3%83%95%E3%83%A9%E3%82%A4%E6%BC%94%E7%AE%97).
butterfly1 :: forall p m. (KnownNat p, PrimMonad m) => UM.MVector (PrimState m) (ModInt p) -> ModInt p -> Int -> Int -> m ()
butterfly1 xs rotN1 iMaxBit iBit = do
  let !nBoxes = bit (iMaxBit - iBit)
  -- interval between pairs
  let !interval = bit (iBit - 1)
  forM_ [0 .. nBoxes - 1] $ \iBox -> do
    (\f -> U.foldM'_ f (1 :: ModInt p) (U.generate (bit (iBit - 1)) id)) $ \rotNK iPair -> do
      -- FIXME: iBox * bit iBit can be combined
      let !i1 = iBox * bit iBit + iPair
      let !i2 = i1 + interval
      !x1 <- UM.read xs i1
      !x2 <- UM.read xs i2
      GM.write xs i1 $! x1 + x2 * rotNK
      GM.write xs i2 $! x1 - x2 * rotNK
      return $ rotNK * rotN1
  where
    !_ = dbgAssert (popCount (UM.length xs) == 1) "not a power of two"

-- | \(\Theta(N \log N)\). Does not contain scaling and index sorting.
invButterfly :: forall p m. (KnownNat p, PrimMonad m) => UM.MVector (PrimState m) (ModInt p) -> m ()
invButterfly xs = do
  let !p = fromInteger (natVal' (proxy# @p)) :: Int
  let !minRot = ModInt @p $ powModConst p (primRoot p) ((p - 1) .>>. iMaxBit)
  let !rots = U.iterateN iMaxBit (\x -> x * x) minRot
  U.iforM_ rots $ \iBit0 rotN1 -> do
    invButterfly1 xs rotN1 iMaxBit (iMaxBit - iBit0)
  where
    !_ = dbgAssert (popCount (UM.length xs) == 1) "not a power of two"
    !iMaxBit = countTrailingZeros $ UM.length xs

-- | \(\Theta(N)\) Inverse butterfly calculation performed in-place.
invButterfly1 :: forall p m. (KnownNat p, PrimMonad m) => UM.MVector (PrimState m) (ModInt p) -> ModInt p -> Int -> Int -> m ()
invButterfly1 xs rotN1 iMaxBit iBit = do
  let !nBoxes = bit (iMaxBit - iBit)
  -- interval between pairs
  let !interval = bit (iBit - 1)
  forM_ [0 .. nBoxes - 1] $ \iBox -> do
    (\f -> U.foldM'_ f (1 :: ModInt p) (U.generate (bit (iBit - 1)) id)) $ \rotNK iPair -> do
      -- FIXME: iBox * bit iBit can be combined
      let !i1 = iBox * bit iBit + iPair
      let !i2 = i1 + interval
      !x1 <- UM.read xs i1
      !x2 <- UM.read xs i2
      GM.write xs i1 $! x1 + x2
      GM.write xs i2 $! (x1 - x2) * rotNK
      return $ rotNK * rotN1
  where
    !_ = dbgAssert (popCount (UM.length xs) == 1) "not a power of two"

-- | Bit reversal up to 32 bits.
-- <https://www.linkedin.com/pulse/%E7%B5%B6%E5%AF%BE%E3%81%AB%E3%82%84%E3%81%A3%E3%81%A6%E3%81%AF%E3%81%84%E3%81%91%E3%81%AA%E3%81%84%E3%83%93%E3%83%83%E3%83%88%E5%8F%8D%E8%BB%A2-masayuki-tatebe?articleId=6539466321338425345>
bitReverse :: Int -> Int
bitReverse x0 =
  let !x1 = ((x0 .&. 0x55555555) .<<. 1) .|. ((x0 .>>. 1) .&. 0x55555555)
      !x2 = ((x1 .&. 0x33333333) .<<. 2) .|. ((x1 .>>. 2) .&. 0x33333333)
      !x3 = ((x2 .&. 0x0F0F0F0F) .<<. 4) .|. ((x2 .>>. 4) .&. 0x0F0F0F0F)
      -- be sure to mask the up 32 bits
      !x4 = ((x3 .<<. 24) .|. ((x3 .&. 0xFF00) .<<. 8) .|. ((x3 .>>. 8) .&. 0xFF00) .|. (x3 .>>. 24)) .&. 0xFFFFFFFF
   in x4

-- | Grow to the length of a power two.
--
-- >>> grow2 $ U.fromList [0 :: Int, 1, 2, 3, 4]
-- [0,1,2,3,4,0,0,0]
grow2 :: forall a. (Num a, U.Unbox a) => U.Vector a -> U.Vector a
grow2 xs
  | U.length xs < len = xs U.++ U.replicate (len - U.length xs) (0 :: a)
  | otherwise = xs
  where
    !len = ceil2 (U.length xs)

-- | Primitive root.
primRoot :: Int -> Int
primRoot 998244353 = 3 -- on wolfarm alpha, PrimitiveRoot[p].
primRoot 754974721 = 11
primRoot 469762049 = 3
primRoot 167772161 = 3
primRoot 2305843009213693951 = 37 -- 2^61 - 1
primRoot 2147483647 = 7 -- 2^31 - 1
primRoot 4294967291 = 2 -- 2^32 - 5
primRoot 2 = 1
primRoot _ = error "TODO: primitive root"
