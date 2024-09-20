{-# LANGUAGE RecordWildCards #-}

-- | Sliding window data structures. See also `Data.Algorithm.SlideMin`.
--
-- = Typical Problems
module Data.Slide where

import Control.Monad.Extra (whenM)
import Control.Monad.Primitive
import Data.Buffer
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Stack

-- | Stack-based sliding window folding, going from left to right only.
-- SWAG (sliding window aggregation).
data StackSlidingFold s a = StackSlidingFold
  { -- | Stack of newly pushed elements, not yet moved to `dualScanrSSF`.
    bufferSSF :: !(Buffer s a),
    -- | Fold of the elements in the buffer.
    bufferFoldSSF :: !(UM.MVector s a),
    -- | Right dual scanning, up to the last popped point. Works great with pop operations.
    dualScanrSSF :: !(Buffer s a)
  }

-- | \(O(N)\)
{-# INLINE newSSF #-}
newSSF :: (PrimMonad m, Monoid a, U.Unbox a) => Int -> m (StackSlidingFold (PrimState m) a)
newSSF n = do
  bufferSSF <- newBuffer n
  bufferFoldSSF <- UM.replicate 1 mempty
  dualScanrSSF <- newBuffer $ n + 1
  pushBack dualScanrSSF mempty
  return StackSlidingFold {..}

-- | \(O(1)\)
{-# INLINE clearSSF #-}
clearSSF :: (PrimMonad m, Monoid a, U.Unbox a) => StackSlidingFold (PrimState m) a -> m ()
clearSSF StackSlidingFold {..} = do
  clearBuffer bufferSSF
  GM.unsafeWrite bufferFoldSSF 0 mempty
  clearBuffer bufferSSF
  pushBack dualScanrSSF mempty

-- | \(O(1)\)
{-# INLINE pushBackSSF #-}
pushBackSSF :: (PrimMonad m, Semigroup a, U.Unbox a) => StackSlidingFold (PrimState m) a -> a -> m ()
pushBackSSF StackSlidingFold {..} !x = do
  GM.unsafeModify bufferFoldSSF (<> x) 0
  pushBack bufferSSF x

-- | Amortised \(O(1)\).
{-# INLINE popFrontSSF #-}
popFrontSSF :: (PrimMonad m, Monoid a, U.Unbox a) => StackSlidingFold (PrimState m) a -> m ()
popFrontSSF StackSlidingFold {..} = do
  -- first element is at the end
  popBack_ dualScanrSSF
  whenM (nullBuffer dualScanrSSF) $ do
    clearBuffer dualScanrSSF
    GM.unsafeWrite bufferFoldSSF 0 mempty

    -- create right dual scanning of the buffer (except the popped one):
    xs <- U.tail <$> unsafeFreezeBuffer bufferSSF
    -- TODO: foldrM can be faster?
    pushBack dualScanrSSF mempty
    U.forM_ (U.reverse xs) $ \fromBack -> do
      end <- readBack dualScanrSSF 0
      pushBack dualScanrSSF $! fromBack <> end

    clearBuffer bufferSSF

-- | \(O(1)\)
{-# INLINE foldSSF #-}
foldSSF :: (PrimMonad m, Semigroup a, U.Unbox a) => StackSlidingFold (PrimState m) a -> m a
foldSSF StackSlidingFold {..} = do
  l <- readBack dualScanrSSF 0
  r <- GM.unsafeRead bufferFoldSSF 0
  return $! l <> r

-- | Dequeue-based sliding window folding. Prefer `StackSlidingFold` for (just a little) speed.
-- SWAG (sliding window aggregation).
data DequeSlidingFold s a = DequeSlidingFold
  { frontBufferDSF :: !(Buffer s a),
    frontScanDSF :: !(Buffer s a),
    backBufferDSF :: !(Buffer s a),
    backScanDSF :: !(Buffer s a)
  }

-- | \(O(N)\)
{-# INLINE newDSF #-}
newDSF :: (PrimMonad m, Monoid a, U.Unbox a) => Int -> m (DequeSlidingFold (PrimState m) a)
newDSF n = do
  frontBufferDSF <- newBufferAsDeque n
  backBufferDSF <- newBufferAsDeque n
  frontScanDSF <- newBuffer $ n + 1
  backScanDSF <- newBuffer $ n + 1
  pushBack frontScanDSF mempty
  pushBack backScanDSF mempty
  return DequeSlidingFold {..}

-- | \(O(1)\)
{-# INLINE clearDSF #-}
clearDSF :: (PrimMonad m, Monoid a, U.Unbox a) => DequeSlidingFold (PrimState m) a -> m ()
clearDSF DequeSlidingFold {..} = do
  clearBuffer frontBufferDSF
  clearBuffer backBufferDSF
  clearBuffer frontScanDSF
  clearBuffer backScanDSF
  pushBack frontScanDSF mempty
  pushBack backScanDSF mempty

-- | \(O(1)\)
{-# INLINE pushFrontDSF #-}
pushFrontDSF :: (HasCallStack, PrimMonad m, Semigroup a, U.Unbox a) => DequeSlidingFold (PrimState m) a -> a -> m ()
pushFrontDSF DequeSlidingFold {..} !x = do
  pushBack frontBufferDSF x
  back <- readBack frontScanDSF 0
  pushBack frontScanDSF $! x <> back

-- | \(O(1)\)
{-# INLINE pushBackDSF #-}
pushBackDSF :: (HasCallStack, PrimMonad m, Semigroup a, U.Unbox a) => DequeSlidingFold (PrimState m) a -> a -> m ()
pushBackDSF DequeSlidingFold {..} !x = do
  pushBack backBufferDSF x
  back <- readBack backScanDSF 0
  pushBack backScanDSF $! back <> x

-- | Amortised \(O(1)\).
{-# INLINE popFrontDSF #-}
popFrontDSF :: (HasCallStack, PrimMonad m, Monoid a, U.Unbox a) => DequeSlidingFold (PrimState m) a -> m ()
popFrontDSF window@DequeSlidingFold {..} = do
  size <- (+) <$> lengthBuffer frontBufferDSF <*> lengthBuffer backBufferDSF
  if size == 1
    then clearDSF window
    else do
      b <- nullBuffer frontBufferDSF
      if b
        then do
          popFront_ backBufferDSF
          balanceDSF window
        else do
          popBack_ frontBufferDSF
          popBack_ frontScanDSF

-- | Amortised \(O(1)\).
{-# INLINE popBackDSF #-}
popBackDSF :: (HasCallStack, PrimMonad m, Monoid a, U.Unbox a) => DequeSlidingFold (PrimState m) a -> m ()
popBackDSF window@DequeSlidingFold {..} = do
  size <- (+) <$> lengthBuffer frontBufferDSF <*> lengthBuffer backBufferDSF
  if size == 1
    then clearDSF window
    else do
      b <- nullBuffer backBufferDSF
      if b
        then do
          popFront_ frontBufferDSF
          balanceDSF window
        else do
          popBack_ backBufferDSF
          popBack_ backScanDSF

-- | \(O(1)\)
{-# INLINE foldDSF #-}
foldDSF :: (HasCallStack, PrimMonad m, Semigroup a, U.Unbox a) => DequeSlidingFold (PrimState m) a -> m a
foldDSF DequeSlidingFold {..} = do
  l <- readBack frontScanDSF 0
  r <- readBack backScanDSF 0
  return $! l <> r

-- | \(O(N)\)
{-# INLINE balanceDSF #-}
balanceDSF :: (HasCallStack, PrimMonad m, Monoid a, U.Unbox a) => DequeSlidingFold (PrimState m) a -> m ()
balanceDSF window@DequeSlidingFold {..} = do
  -- be sure to revere!
  xs <- (U.++) <$> (U.reverse <$> unsafeFreezeBuffer frontBufferDSF) <*> unsafeFreezeBuffer backBufferDSF
  let (!xsL, !xsR) = U.splitAt (U.length xs `div` 2) xs
  clearDSF window
  -- be sure to revere!
  U.forM_ (U.reverse xsL) $ pushFrontDSF window
  U.forM_ xsR $ pushBackDSF window
