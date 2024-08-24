{-# LANGUAGE RecordWildCards #-}

-- | Sliding window data structures. See also `Data.Algorithm.SlideMin`.
--
-- = Typical Problems
module Data.Slide where

import Control.Monad (when)
import Control.Monad.Extra (whenM, whenJustM)
import Control.Monad.Fix
import Control.Monad.Primitive
import Data.Buffer
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- | Stack-based sliding window folding from left to right only. SWAG (sliding window aggregation).
data SlidingFold s a = SlidingFold
  { -- | Stack of newly pushed elements, not yet moved to `dualScanrSF`.
    bufferSF :: !(Buffer s a),
    -- | Fold of the elements in the buffer.
    bufferFoldSF :: !(UM.MVector s a),
    -- | Right dual scanning, up to the last popped point. Works great with pop operations.
    dualScanrSF :: !(Buffer s a)
  }

-- | \(O(N)\)
newSF :: (PrimMonad m, Monoid a, U.Unbox a) => Int -> m (SlidingFold (PrimState m) a)
newSF n = do
  bufferSF <- newBuffer n
  dualScanrSF <- newBuffer $ n + 1
  pushBack dualScanrSF mempty
  bufferFoldSF <- UM.replicate 1 mempty
  return SlidingFold {..}

-- | \(O(1)\)
pushBackSF :: (PrimMonad m, Semigroup a, U.Unbox a) => SlidingFold (PrimState m) a -> a -> m ()
pushBackSF SlidingFold {..} !x = do
  GM.unsafeModify bufferFoldSF (<> x) 0
  pushBack bufferSF x

-- | Amortised \(O(1)\).
popFrontSF :: (PrimMonad m, Monoid a, U.Unbox a) => SlidingFold (PrimState m) a -> m ()
popFrontSF SlidingFold {..} = do
  -- first element is at the end
  popBack_ dualScanrSF
  whenM (nullBuffer dualScanrSF) $ do
    clearBuffer dualScanrSF
    GM.unsafeWrite bufferFoldSF 0 mempty

    -- create right dual scanning of the buffer (except the popped one):
    xs <- U.tail <$> unsafeFreezeBuffer bufferSF
    -- TODO: foldrM can be faster?
    pushBack dualScanrSF mempty
    U.forM_ (U.reverse xs) $ \fromBack -> do
      end <- readBack dualScanrSF 0
      pushBack dualScanrSF $! fromBack <> end

    clearBuffer bufferSF

-- | \(O(1)\)
foldSF :: (PrimMonad m, Semigroup a, U.Unbox a) => SlidingFold (PrimState m) a -> m a
foldSF SlidingFold {..} = do
  l <- readBack dualScanrSF 0
  r <- GM.unsafeRead bufferFoldSF 0
  return $! l <> r
