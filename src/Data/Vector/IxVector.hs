{-# LANGUAGE RecordWildCards #-}

-- | `Ix`-based API over `vector`.
module Data.Vector.IxVector where

import Control.Monad (forM_)
import Control.Monad.Primitive
import Data.Ix
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU

-- | N-dimensional @Vector@ or @MVector@ with `Data.Ix`.
data IxVector i v = IxVector {boundsIV :: !(i, i), vecIV :: !v}
  deriving (Show, Eq)

-- | Partial `IxVector` accessor
(@!) :: (Ix i, VG.Vector v a) => IxVector i (v a) -> i -> a
(@!) IxVector {..} i = vecIV VG.! index boundsIV i

-- | Total `IxVector` accessor
(@!?) :: (Ix i, VG.Vector v a) => IxVector i (v a) -> i -> Maybe a
(@!?) IxVector {..} i
  -- NOTE: `index` panics on out of range
  | inRange boundsIV i = Just (vecIV VG.! (index boundsIV i))
  | otherwise = Nothing

-- TODO: `createIx` where we freeze the `IxVector`
-- createIx :: (VG.Vector v a, Ix i) => (forall s. ST s (Mutable v s a)) -> IxVector i (v a)
-- createIx = runST

-- replicateIVM

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

-- | WARNING: Can you really allocate/run \(O(HW)\) algorithm?
imos2DIV :: IxVector (Int, Int) (VU.Vector Int) -> IxVector (Int, Int) (VU.Vector Int)
imos2DIV !seeds@IxVector {boundsIV} = IxVector boundsIV $ VU.create $ do
  !vec <- IxVector boundsIV <$> VU.thaw (vecIV seeds)

  let (!minY, !minX) = fst boundsIV

  -- row scan
  forM_ (range boundsIV) $ \(!y, !x) -> do
    !v <- if x == minX then return 0 else readIV vec (y, x - 1)
    modifyIV vec (+ v) (y, x)

  -- column scan
  forM_ (range boundsIV) $ \(!x, !y) -> do
    !v <- if y == minY then return 0 else readIV vec (y - 1, x)
    modifyIV vec (+ v) (y, x)

  return $ vecIV vec
