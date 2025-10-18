{-# LANGUAGE RecordWildCards #-}

-- | Sliding window data structures. See also `Data.Algorithm.SlideMin`.
--
-- = Typical Problems
module Data.Slide where

import qualified AtCoder.Internal.Queue as Q
import Control.Monad.Extra (whenM)
import Control.Monad.Primitive
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Stack

-- | Stack-based sliding window folding, going from left to right only.
-- SWAG (sliding window aggregation).
data StackSlidingFold s a = StackSlidingFold
  { -- | Stack of newly pushed elements, not yet moved to `dualScanrSSF`.
    bufferSSF :: !(Q.Queue s a),
    -- | Fold of the elements in the buffer.
    bufferFoldSSF :: !(UM.MVector s a),
    -- | Right dual scanning, up to the last popped point. Works great with pop operations.
    dualScanrSSF :: !(Q.Queue s a)
  }

-- | \(O(N)\)
{-# INLINE newSSF #-}
newSSF :: (PrimMonad m, Monoid a, U.Unbox a) => Int -> m (StackSlidingFold (PrimState m) a)
newSSF n = do
  bufferSSF <- Q.new n
  bufferFoldSSF <- UM.replicate 1 mempty
  dualScanrSSF <- Q.new $ n + 1
  Q.pushBack dualScanrSSF mempty
  pure StackSlidingFold {..}

-- | \(O(1)\)
{-# INLINE clearSSF #-}
clearSSF :: (PrimMonad m, Monoid a, U.Unbox a) => StackSlidingFold (PrimState m) a -> m ()
clearSSF StackSlidingFold {..} = do
  Q.clear bufferSSF
  GM.unsafeWrite bufferFoldSSF 0 mempty
  Q.clear bufferSSF
  Q.pushBack dualScanrSSF mempty

-- | \(O(1)\)
{-# INLINE pushBackSSF #-}
pushBackSSF :: (PrimMonad m, Semigroup a, U.Unbox a) => StackSlidingFold (PrimState m) a -> a -> m ()
pushBackSSF StackSlidingFold {..} !x = do
  GM.unsafeModify bufferFoldSSF (<> x) 0
  Q.pushBack bufferSSF x

-- | Amortized \(O(1)\).
{-# INLINE popFrontSSF #-}
popFrontSSF :: (PrimMonad m, Monoid a, U.Unbox a) => StackSlidingFold (PrimState m) a -> m ()
popFrontSSF StackSlidingFold {..} = do
  -- first element is at the end
  Q.popBack_ dualScanrSSF
  whenM (Q.null dualScanrSSF) $ do
    Q.clear dualScanrSSF
    GM.unsafeWrite bufferFoldSSF 0 mempty

    -- create right dual scanning of the buffer (except the popped one):
    xs <- U.tail <$> Q.unsafeFreeze bufferSSF
    -- TODO: foldrM can be faster?
    Q.pushBack dualScanrSSF mempty
    U.forM_ (U.reverse xs) $ \fromBack -> do
      end <- Q.readBack dualScanrSSF 0
      Q.pushBack dualScanrSSF $! fromBack <> end

    Q.clear bufferSSF

-- | \(O(1)\)
{-# INLINE foldSSF #-}
foldSSF :: (PrimMonad m, Semigroup a, U.Unbox a) => StackSlidingFold (PrimState m) a -> m a
foldSSF StackSlidingFold {..} = do
  l <- Q.readBack dualScanrSSF 0
  r <- GM.unsafeRead bufferFoldSSF 0
  pure $! l <> r

-- | Dequeue-based sliding window folding. Prefer `StackSlidingFold` for (just a little) speed.
-- SWAG (sliding window aggregation).
data DequeSlidingFold s a = DequeSlidingFold
  { frontBufferDSF :: !(Q.Queue s a),
    frontScanDSF :: !(Q.Queue s a),
    backBufferDSF :: !(Q.Queue s a),
    backScanDSF :: !(Q.Queue s a)
  }

-- | \(O(N)\)
{-# INLINE newDSF #-}
newDSF :: (PrimMonad m, Monoid a, U.Unbox a) => Int -> m (DequeSlidingFold (PrimState m) a)
newDSF n = do
  frontBufferDSF <- Q.newDeque n
  backBufferDSF <- Q.newDeque n
  frontScanDSF <- Q.new $ n + 1
  backScanDSF <- Q.new $ n + 1
  Q.pushBack frontScanDSF mempty
  Q.pushBack backScanDSF mempty
  pure DequeSlidingFold {..}

-- | \(O(1)\)
{-# INLINE clearDSF #-}
clearDSF :: (PrimMonad m, Monoid a, U.Unbox a) => DequeSlidingFold (PrimState m) a -> m ()
clearDSF DequeSlidingFold {..} = do
  Q.clear frontBufferDSF
  Q.clear backBufferDSF
  Q.clear frontScanDSF
  Q.clear backScanDSF
  Q.pushBack frontScanDSF mempty
  Q.pushBack backScanDSF mempty

-- | \(O(1)\)
{-# INLINE pushFrontDSF #-}
pushFrontDSF :: (HasCallStack, PrimMonad m, Semigroup a, U.Unbox a) => DequeSlidingFold (PrimState m) a -> a -> m ()
pushFrontDSF DequeSlidingFold {..} !x = do
  Q.pushBack frontBufferDSF x
  back <- Q.readBack frontScanDSF 0
  Q.pushBack frontScanDSF $! x <> back

-- | \(O(1)\)
{-# INLINE pushBackDSF #-}
pushBackDSF :: (HasCallStack, PrimMonad m, Semigroup a, U.Unbox a) => DequeSlidingFold (PrimState m) a -> a -> m ()
pushBackDSF DequeSlidingFold {..} !x = do
  Q.pushBack backBufferDSF x
  back <- Q.readBack backScanDSF 0
  Q.pushBack backScanDSF $! back <> x

-- | Amortized \(O(1)\).
{-# INLINE popFrontDSF #-}
popFrontDSF :: (HasCallStack, PrimMonad m, Monoid a, U.Unbox a) => DequeSlidingFold (PrimState m) a -> m ()
popFrontDSF window@DequeSlidingFold {..} = do
  size <- (+) <$> Q.length frontBufferDSF <*> Q.length backBufferDSF
  if size == 1
    then clearDSF window
    else do
      b <- Q.null frontBufferDSF
      if b
        then do
          Q.popFront_ backBufferDSF
          balanceDSF window
        else do
          Q.popBack_ frontBufferDSF
          Q.popBack_ frontScanDSF

-- | Amortized \(O(1)\).
{-# INLINE popBackDSF #-}
popBackDSF :: (HasCallStack, PrimMonad m, Monoid a, U.Unbox a) => DequeSlidingFold (PrimState m) a -> m ()
popBackDSF window@DequeSlidingFold {..} = do
  size <- (+) <$> Q.length frontBufferDSF <*> Q.length backBufferDSF
  if size == 1
    then clearDSF window
    else do
      b <- Q.null backBufferDSF
      if b
        then do
          Q.popFront_ frontBufferDSF
          balanceDSF window
        else do
          Q.popBack_ backBufferDSF
          Q.popBack_ backScanDSF

-- | \(O(1)\)
{-# INLINE foldDSF #-}
foldDSF :: (HasCallStack, PrimMonad m, Semigroup a, U.Unbox a) => DequeSlidingFold (PrimState m) a -> m a
foldDSF DequeSlidingFold {..} = do
  l <- Q.readBack frontScanDSF 0
  r <- Q.readBack backScanDSF 0
  pure $! l <> r

-- | \(O(N)\)
{-# INLINE balanceDSF #-}
balanceDSF :: (HasCallStack, PrimMonad m, Monoid a, U.Unbox a) => DequeSlidingFold (PrimState m) a -> m ()
balanceDSF window@DequeSlidingFold {..} = do
  -- be sure to revere!
  xs <- (U.++) <$> (U.reverse <$> Q.unsafeFreeze frontBufferDSF) <*> Q.unsafeFreeze backBufferDSF
  let (!xsL, !xsR) = U.splitAt (U.length xs `div` 2) xs
  clearDSF window
  -- be sure to revere!
  U.forM_ (U.reverse xsL) $ pushFrontDSF window
  U.forM_ xsR $ pushBackDSF window
