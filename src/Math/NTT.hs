{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}

-- | WIP Number-theoretic transform (integer DFT).
module Math.NTT where

import Control.Monad (forM_)
import Control.Monad.ST
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bits
import Data.ModInt
import Data.Ord (Down (..), comparing)
import qualified Data.Vector.Algorithms.Intro as VAI
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word
import Debug.Trace
import GHC.Exts (proxy#)
import GHC.TypeLits
import Math.BitSet (ceil2)
import Math.PowMod (powModConst)
import ToyLib.Debug
import ToyLib.Debug (dbgAssert)
import Unsafe.Coerce

bitRevSort :: (G.Vector v a, G.Vector v (Int, a)) => v a -> v a
bitRevSort =
  G.map snd
    . G.modify (VAI.sortBy (comparing (bitReverse . fst)))
    . G.indexed

-- | \(\Theta(N \log N)\) Number-theoretic transform (integer DFT).
-- TODO: re-sorting the output?
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

-- | \(\Theta(N \log N)\) Convolution.
--
-- \(\mathcal{F}[f*g](\omega) = \mathcal{F}[f](\omega) \mathcal{F}[g](\omega)\).
--
-- \(\mathbb{a} * \mathbb{b} = \mathcal{F^{-1}}(\mathcal{F}(\mathbb{a}) \mathcal{F}(\mathbb{b}))\)
--
-- >>> :set -XDataKinds
-- >>> convolute @998244353 (U.fromList [1, 1, 1, 0]) (U.fromList [1, 1, 1, 0])
-- [1,2,3,2,1,0,0]
-- >>> convolute @998244353 (U.fromList [1, 1, 1]) (U.fromList [1, 1, 1, 0])
-- [1,2,3,2,1,0]
convolute :: forall p. (KnownNat p) => U.Vector (ModInt p) -> U.Vector (ModInt p) -> U.Vector (ModInt p)
convolute xs1 xs2
  | U.length xs1 < U.length xs2 = convolute xs2 xs1
convolute xs1 xs2 = runST $ do
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

-- | \(\Theta(N \log N)\)
butterfly :: forall p m. (KnownNat p, PrimMonad m) => UM.MVector (PrimState m) (ModInt p) -> m ()
butterfly xs = do
  -- g^{p-1/2^m} = 1 \pmod p
  let !p = fromInteger (natVal' (proxy# @p)) :: Int
  let !minRot = ModInt @p $ powModConst p (primRoot p) ((p - 1) .>>. iMaxBit)
  -- let !_ = traceShow ("minRot", minRot) ()
  let !rots = U.iterateN iMaxBit (\x -> x * x) minRot
  -- let !_ = traceShow ("rots", rots) ()
  U.iforM_ (U.reverse rots) $ \iBit0 rotN1 -> do
    xs' <- U.unsafeFreeze xs
    -- let !_ = traceShow xs' ()
    -- let !_ = traceShow (iBit0, rotN1)
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
  -- let !_ = traceShow rotN1 ()
  forM_ [0 .. nBoxes - 1] $ \iBox -> do
    (\f -> U.foldM'_ f (1 :: ModInt p) (U.generate (bit (iBit - 1)) id)) $ \rotNK iPair -> do
      -- FIXME: iBox * bit iBit can be combined
      let !i1 = iBox * bit iBit + iPair
      let !i2 = i1 + interval
      !x1 <- UM.read xs i1
      !x2 <- UM.read xs i2
      -- let !_ = traceShow (iBox, iPair, i1, i2) ()
      UM.write xs i1 $! x1 + x2 * rotNK
      UM.write xs i2 $! x1 - x2 * rotNK
      -- let !_ = traceShow (rotNK, (x1, x2), (x1 + x2 * rotNK, x1 - x2 * rotNK)) ()
      return $ rotNK * rotN1
  where
    !_ = dbgAssert (popCount (UM.length xs) == 1) "not a power of two"

-- | \(\Theta(N \log N)\). Does not contain scaling and index sorting.
invButterfly :: forall p m. (KnownNat p, PrimMonad m) => UM.MVector (PrimState m) (ModInt p) -> m ()
invButterfly xs = do
  let !p = fromInteger (natVal' (proxy# @p)) :: Int
  let !minRot = ModInt @p $ powModConst p (primRoot p) ((p - 1) .>>. iMaxBit)
  -- let !_ = traceShow ("minRot", minRot) ()
  -- FIXME: I think `recip` should go to the fowarding `butterfly`
  let !rots = U.map recip $ U.iterateN iMaxBit (\x -> x * x) minRot
  U.iforM_ rots $ \iBit0 rotN1 -> do
    xs' <- U.unsafeFreeze xs
    -- let !_ = traceShow xs' ()
    -- let !_ = traceShow (iBit0, rotN1 )
    invButterfly1 xs rotN1 iMaxBit (iMaxBit - iBit0)
  where
    -- TODO: validate the number of digits
    !_ = dbgAssert (popCount (UM.length xs) == 1) "not a power of two"
    !iMaxBit = countTrailingZeros $ UM.length xs

-- | \(\Theta(N)\) Inverse butterfly calculation performed in-place.
invButterfly1 :: forall p m. (KnownNat p, PrimMonad m) => UM.MVector (PrimState m) (ModInt p) -> ModInt p -> Int -> Int -> m ()
invButterfly1 xs rotN1 iMaxBit iBit = do
  let !nBoxes = bit (iMaxBit - iBit)
  -- interval between pairs
  let !interval = bit (iBit - 1)
  -- let !_ = traceShow ("counts", nBoxes, bit (iBit - 1) :: Int) ()
  -- FIXME: replace with rangeUR or enumFromTo
  forM_ [0 .. nBoxes - 1] $ \iBox -> do
    -- let !_ = traceShow ("box", iBox) ()
    (\f -> U.foldM'_ f (1 :: ModInt p) (U.generate (bit (iBit - 1)) id)) $ \rotNK iPair -> do
      -- FIXME: iBox * bit iBit can be combined
      let !i1 = iBox * bit iBit + iPair
      let !i2 = i1 + interval
      !x1 <- UM.read xs i1
      !x2 <- UM.read xs i2
      -- let !_ = traceShow (i1, i2) ()
      UM.write xs i1 $! x1 + x2
      UM.write xs i2 $! (x1 - x2) * rotNK
      -- let !_ = traceShow (iBit, rotNK, (x1, x2), (x1 + x2, (x1 - x2) * rotNK)) ()
      return $ rotNK * rotN1
  where
    !_ = dbgAssert (popCount (UM.length xs) == 1) "not a power of two"

-- | https://www.linkedin.com/pulse/%E7%B5%B6%E5%AF%BE%E3%81%AB%E3%82%84%E3%81%A3%E3%81%A6%E3%81%AF%E3%81%84%E3%81%91%E3%81%AA%E3%81%84%E3%83%93%E3%83%83%E3%83%88%E5%8F%8D%E8%BB%A2-masayuki-tatebe?articleId=6539466321338425345
bitReverse :: Int -> Int
bitReverse x0 =
  let !x1 = (x0 .&. 0x55555555) .<<. 1 .|. (x0 .>>. 1) .&. 0x55555555
      !x2 = (x1 .&. 0x33333333) .<<. 2 .|. (x1 .>>. 2) .&. 0x33333333
      !x3 = (x2 .&. 0x0F0F0F0F) .<<. 4 .|. (x2 .>>. 4) .&. 0x0F0F0F0F
      !x4 = (x3 .<<. 24) .|. ((x3 .&. 0xFF00) .<<. 8) .|. ((x3 .>>. 8) .&. 0xFF00) .|. (x3 .>>. 24)
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
primRoot 998244353 = 3
primRoot 469762049 = 3
primRoot 2 = 1
primRoot _ = error "TODO: primitive root"
