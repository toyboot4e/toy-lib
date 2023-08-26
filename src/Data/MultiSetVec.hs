{-# LANGUAGE LambdaCase #-}

-- | Dense, mutable multi set.
module Data.MultiSetVec where

import Control.Monad.Primitive
import Data.Primitive.MutVar
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

-- | Dense, mutable multi set.
data MultiSetVec s = MultiSetVec (MutVar s Int) (VUM.MVector s Int)

-- | Monadic `show` over `MultiSetVec`.
showMSV :: (PrimMonad m) => MultiSetVec (PrimState m) -> m String
showMSV (MultiSetVec !nRef !mVec) = do
  !n <- readMutVar nRef
  !vec <- VG.unsafeFreeze mVec
  return $ show (n, vec)

newMSV :: (PrimMonad m) => Int -> m (MultiSetVec (PrimState m))
newMSV !capacity = MultiSetVec <$> newMutVar (0 :: Int) <*> VUM.replicate capacity (0 :: Int)

-- -- | WARNING: Any read/write will result im runtime error. Use `clearMSV` if it's accessed again.
-- emptyMSV :: (PrimMonad m) => m (MultiSetVec (PrimState m))
-- emptyMSV = newMSV 0

clearMSV :: (PrimMonad m) => MultiSetVec (PrimState m) -> m ()
clearMSV (MultiSetVec !nRef !mVec) = do
  writeMutVar nRef 0
  VGM.set mVec 0

fromVecMSV :: (PrimMonad m) => Int -> VU.Vector Int -> m (MultiSetVec (PrimState m))
fromVecMSV !capacity !xs = do
  !msv <- newMSV capacity
  VU.forM_ xs (incMSV msv)
  return msv

countMSV :: (PrimMonad m) => MultiSetVec (PrimState m) -> m Int
countMSV (MultiSetVec !nRef !_) = readMutVar nRef

nullMSV :: (PrimMonad m) => MultiSetVec (PrimState m) -> m Bool
nullMSV = fmap (== 0) . countMSV

readMSV :: (PrimMonad m) => MultiSetVec (PrimState m) -> Int -> m Int
readMSV (MultiSetVec !_ !mVec) = VGM.read mVec

incMSV :: (PrimMonad m) => MultiSetVec (PrimState m) -> Int -> m ()
incMSV (MultiSetVec !nRef !mVec) k =
  VGM.read mVec k >>= \case
    0 -> do
      modifyMutVar' nRef succ
      VGM.write mVec k 1
    !nk -> do
      VGM.write mVec k (nk + 1)

decMSV :: (PrimMonad m) => MultiSetVec (PrimState m) -> Int -> m ()
decMSV (MultiSetVec !nRef !mVec) k =
  VGM.read mVec k >>= \case
    0 -> return ()
    1 -> do
      modifyMutVar' nRef pred
      VGM.write mVec k 0
    !nk -> do
      VGM.write mVec k (nk - 1)

-- | $O(n)$ Find minimum key element
minMSV :: (PrimMonad m) => MultiSetVec (PrimState m) -> m (Maybe (Int, Int))
minMSV (MultiSetVec !nRef !mVec) =
  readMutVar nRef >>= \case
    0 -> return Nothing
    _ -> do
      !vec <- VG.unsafeFreeze mVec
      return . fmap (\i -> (i, vec VG.! i)) $ VG.findIndex (> 0) vec

-- | $O(n)$ Find maximum key element
maxMSV :: (PrimMonad m) => MultiSetVec (PrimState m) -> m (Maybe (Int, Int))
maxMSV (MultiSetVec !nRef !mVec) =
  readMutVar nRef >>= \case
    0 -> return Nothing
    _ -> do
      !vec <- VG.unsafeFreeze mVec
      return . fmap (\i -> (i, vec VG.! i)) $ VG.findIndexR (> 0) vec

unsafeFreezeMSV :: (PrimMonad m) => MultiSetVec (PrimState m) -> m (Int, VU.Vector Int)
unsafeFreezeMSV (MultiSetVec !nRef !mVec) = (,) <$> readMutVar nRef <*> VU.unsafeFreeze mVec

