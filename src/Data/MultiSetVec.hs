{-# LANGUAGE LambdaCase #-}

-- | Dense, mutable multi set.
--
-- = Typical problems
-- - [ABC 315 D - Magical Cookies](https://atcoder.jp/contests/abc315/tasks/abc315_d)
module Data.MultiSetVec where

import Control.Monad.Primitive
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

-- | Dense, mutable multi set.
data MultiSetVec s = MultiSetVec (UM.MVector s Int) (UM.MVector s Int)

-- | Monadic `show` over `MultiSetVec`.
show :: (PrimMonad m) => MultiSetVec (PrimState m) -> m String
show (MultiSetVec !nRef !mVec) = do
  !n <- UM.unsafeRead nRef 0
  !vec <- G.unsafeFreeze mVec
  pure $ Prelude.show (n, vec)

-- | \(O(N)\)
new :: (PrimMonad m) => Int -> m (MultiSetVec (PrimState m))
new !capacity = MultiSetVec <$> UM.replicate 1 (0 :: Int) <*> UM.replicate capacity (0 :: Int)

-- -- | WARNING: Any read/write will result im runtime error. Use `clear` if it's accessed again.
-- empty :: (PrimMonad m) => m (MultiSetVec (PrimState m))
-- empty = new 0

-- | \(O(N)\)
clear :: (PrimMonad m) => MultiSetVec (PrimState m) -> m ()
clear (MultiSetVec !nRef !mVec) = do
  UM.unsafeWrite nRef 0 0
  GM.set mVec 0

fromVec :: (PrimMonad m) => Int -> U.Vector Int -> m (MultiSetVec (PrimState m))
fromVec !capacity !xs = do
  !msv <- new capacity
  U.forM_ xs (inc msv)
  pure msv

-- | \(O(1)\) Returns the number of keys in the multiset.
count :: (PrimMonad m) => MultiSetVec (PrimState m) -> m Int
count (MultiSetVec !nRef !_) = UM.unsafeRead nRef 0

-- | \(O(1)\)
null :: (PrimMonad m) => MultiSetVec (PrimState m) -> m Bool
null = fmap (== 0) . count

-- | \(O(1)\)
read :: (PrimMonad m) => MultiSetVec (PrimState m) -> Int -> m Int
read (MultiSetVec !_ !mVec) = GM.read mVec

-- | \(O(1)\)
inc :: (PrimMonad m) => MultiSetVec (PrimState m) -> Int -> m ()
inc (MultiSetVec !nRef !mVec) k =
  GM.read mVec k >>= \case
    0 -> do
      UM.unsafeModify nRef (+ 1) 0
      GM.write mVec k 1
    !nk -> do
      GM.write mVec k (nk + 1)

-- | \(O(1)\)
dec :: (PrimMonad m) => MultiSetVec (PrimState m) -> Int -> m ()
dec (MultiSetVec !nRef !mVec) k =
  GM.read mVec k >>= \case
    0 -> pure ()
    1 -> do
      UM.unsafeModify nRef (subtract 1) 0
      GM.write mVec k 0
    !nk -> do
      GM.write mVec k (nk - 1)

-- | \(O(N)\) Finds the minimum key element
min :: (PrimMonad m) => MultiSetVec (PrimState m) -> m (Maybe (Int, Int))
min (MultiSetVec !nRef !mVec) =
  UM.unsafeRead nRef 0 >>= \case
    0 -> pure Nothing
    _ -> do
      !vec <- G.unsafeFreeze mVec
      pure . fmap (\i -> (i, vec G.! i)) $ G.findIndex (> 0) vec

-- | \(O(N)\) Finds the maximum key element
max :: (PrimMonad m) => MultiSetVec (PrimState m) -> m (Maybe (Int, Int))
max (MultiSetVec !nRef !mVec) =
  UM.unsafeRead nRef 0 >>= \case
    0 -> pure Nothing
    _ -> do
      !vec <- G.unsafeFreeze mVec
      pure . fmap (\i -> (i, vec G.! i)) $ G.findIndexR (> 0) vec

-- | \(O(N)\)
unsafeFreeze :: (PrimMonad m) => MultiSetVec (PrimState m) -> m (Int, U.Vector Int)
unsafeFreeze (MultiSetVec !nRef !mVec) = (,) <$> UM.unsafeRead nRef 0 <*> U.unsafeFreeze mVec
