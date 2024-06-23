{-# LANGUAGE LambdaCase #-}

-- | Dense, mutable multi set.
--
-- = Typical problems
-- - [ABC 315 D - Magical Cookies](https://atcoder.jp/contests/abc315/tasks/abc315_d)
module Data.MultiSetVec where

import Control.Monad.Primitive
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- | Dense, mutable multi set.
data MultiSetVec s = MultiSetVec (UM.MVector s Int) (UM.MVector s Int)

-- | Monadic `show` over `MultiSetVec`.
showMSV :: (PrimMonad m) => MultiSetVec (PrimState m) -> m String
showMSV (MultiSetVec !nRef !mVec) = do
  !n <- UM.unsafeRead nRef 0
  !vec <- G.unsafeFreeze mVec
  return $ show (n, vec)

-- | \(O(N)\)
newMSV :: (PrimMonad m) => Int -> m (MultiSetVec (PrimState m))
newMSV !capacity = MultiSetVec <$> UM.replicate 1 (0 :: Int) <*> UM.replicate capacity (0 :: Int)

-- -- | WARNING: Any read/write will result im runtime error. Use `clearMSV` if it's accessed again.
-- emptyMSV :: (PrimMonad m) => m (MultiSetVec (PrimState m))
-- emptyMSV = newMSV 0

-- | \(O(N)\)
clearMSV :: (PrimMonad m) => MultiSetVec (PrimState m) -> m ()
clearMSV (MultiSetVec !nRef !mVec) = do
  UM.unsafeWrite nRef 0 0
  GM.set mVec 0

fromVecMSV :: (PrimMonad m) => Int -> U.Vector Int -> m (MultiSetVec (PrimState m))
fromVecMSV !capacity !xs = do
  !msv <- newMSV capacity
  U.forM_ xs (incMSV msv)
  return msv

-- | \(O(1)\) Returns the number of keys in the multiset.
countMSV :: (PrimMonad m) => MultiSetVec (PrimState m) -> m Int
countMSV (MultiSetVec !nRef !_) = UM.unsafeRead nRef 0

-- | \(O(1)\)
nullMSV :: (PrimMonad m) => MultiSetVec (PrimState m) -> m Bool
nullMSV = fmap (== 0) . countMSV

-- | \(O(1)\)
readMSV :: (PrimMonad m) => MultiSetVec (PrimState m) -> Int -> m Int
readMSV (MultiSetVec !_ !mVec) = GM.read mVec

-- | \(O(1)\)
incMSV :: (PrimMonad m) => MultiSetVec (PrimState m) -> Int -> m ()
incMSV (MultiSetVec !nRef !mVec) k =
  GM.read mVec k >>= \case
    0 -> do
      UM.unsafeModify nRef (+ 1) 0
      GM.write mVec k 1
    !nk -> do
      GM.write mVec k (nk + 1)

-- | \(O(1)\)
decMSV :: (PrimMonad m) => MultiSetVec (PrimState m) -> Int -> m ()
decMSV (MultiSetVec !nRef !mVec) k =
  GM.read mVec k >>= \case
    0 -> return ()
    1 -> do
      UM.unsafeModify nRef (subtract 1) 0
      GM.write mVec k 0
    !nk -> do
      GM.write mVec k (nk - 1)

-- | \(O(N)\) Finds the minimum key element
minMSV :: (PrimMonad m) => MultiSetVec (PrimState m) -> m (Maybe (Int, Int))
minMSV (MultiSetVec !nRef !mVec) =
  UM.unsafeRead nRef 0 >>= \case
    0 -> return Nothing
    _ -> do
      !vec <- G.unsafeFreeze mVec
      return . fmap (\i -> (i, vec G.! i)) $ G.findIndex (> 0) vec

-- | \(O(N)\) Finds the maximum key element
maxMSV :: (PrimMonad m) => MultiSetVec (PrimState m) -> m (Maybe (Int, Int))
maxMSV (MultiSetVec !nRef !mVec) =
  UM.unsafeRead nRef 0 >>= \case
    0 -> return Nothing
    _ -> do
      !vec <- G.unsafeFreeze mVec
      return . fmap (\i -> (i, vec G.! i)) $ G.findIndexR (> 0) vec

-- | \(O(N)\)
unsafeFreezeMSV :: (PrimMonad m) => MultiSetVec (PrimState m) -> m (Int, U.Vector Int)
unsafeFreezeMSV (MultiSetVec !nRef !mVec) = (,) <$> UM.unsafeRead nRef 0 <*> U.unsafeFreeze mVec
