{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

-- | `Ix`-based API over `vector`.
module Data.Vector.IxVector where

import Control.Monad.Primitive
import Data.Ix
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM

-- | N-dimensional @Vector@ or @MVector@ with `Data.Ix`.
data IxVector i v = IxVector {boundsIV :: !(i, i), vecIV :: !v}

(.!) :: (Ix i, VG.Vector v a) => IxVector i (v a) -> i -> a
(.!) IxVector {..} i = vecIV VG.! (index boundsIV i)

-- | Reads a value from `IxVector`.
{-# INLINE readIV #-}
readIV :: (Ix i, PrimMonad m, VGM.MVector v a) => IxVector i (v (PrimState m) a) -> i -> m a
readIV IxVector {..} i = VGM.read vecIV (index boundsIV i)

-- | Writes a value to `IxVector`.
{-# INLINE writeIV #-}
writeIV :: (Ix i, PrimMonad m, VGM.MVector v a) => IxVector i (v (PrimState m) a) -> i -> a -> m ()
writeIV IxVector {..} i a = VGM.write vecIV (index boundsIV i) a

{-# INLINE modifyIV #-}
modifyIV :: (Ix i, PrimMonad m, VGM.MVector v a) => IxVector i (v (PrimState m) a) -> (a -> a) -> i -> m ()
modifyIV IxVector {..} !alter i = VGM.modify vecIV alter (index boundsIV i)

{-# INLINE swapIV #-}
swapIV :: (Ix i, PrimMonad m, VGM.MVector v a) => IxVector i (v (PrimState m) a) -> i -> i -> m ()
swapIV IxVector {..} !i1 !i2 = VGM.swap vecIV (index boundsIV i1) (index boundsIV i2)

