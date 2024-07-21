{-# LANGUAGE RecordWildCards #-}

-- | Dense int set or a 64-ary tree that covers @[0, n)@.
--
-- <https://github.com/maspypy/library/blob/main/ds/fastset.hpp>
module Data.DenseIntSet where

import Control.Monad
import Control.Monad.Extra (unlessM, whenM)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bifunctor (bimap)
import Data.Bits
import Data.Ix
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Stack (HasCallStack)
import Math.BitSet (lsbOf, msbOf)
import ToyLib.Macro

-- | Dense int set or a W-ary tree where @W = wordDIS@.
wordDIS :: Int
wordDIS = 64

-- | Dense int set or a 64-ary tree that covers @[0, n)@.
-- = (Internal) segments
--
-- The idea is very similar to [hibitset](https://docs.rs/hibitset/0.6.4/hibitset/index.html), with
-- top and bottom reversed.
--
-- @
-- [.][.][.][.][.][.][.][.] ..  Layer 0: dense bits
-- [..........][..........] ..  Layer 1: sparse (1/64)
-- [......................] ..  Layer 2: more sparse (1/64^2)
-- @
data DenseIntSet s = DenseIntSet
  { -- | Maximum number of elements.
    capacityDIS :: !Int,
    -- | The number of elements.
    sizeDIS_ :: !(UM.MVector s Int),
    -- | Segments.
    vecDIS :: !(V.Vector (UM.MVector s Int))
  }

-- | \(O(N \log N)\) Creates a new `DenseIntSet` that covers @[0, n)@.
newDIS :: (PrimMonad m) => Int -> m (DenseIntSet (PrimState m))
newDIS capacityDIS = do
  vecDIS <-
    V.unfoldrExactNM
      (max 1 logSize)
      ( \len -> do
          let !len' = (len + wordDIS - 1) `div` wordDIS
          (,len') <$> UM.replicate len' 0
      )
      capacityDIS
  sizeDIS_ <- UM.replicate 1 (0 :: Int)
  return DenseIntSet {..}
  where
    (!_, !logSize) =
      until
        ((<= 1) . fst)
        (bimap ((`div` wordDIS) . (+ (wordDIS - 1))) (+ 1))
        (capacityDIS, 0)

-- buildDIS :: (PrimMonad m) => Int -> (Int -> Int) -> m (DenseIntSet (PrimState m))
-- buildDIS capacityDIS f = runST $ do

-- | (Internal) Keys out of the range are reported as runtime error.
validateKeyDIS :: (HasCallStack) => String -> DenseIntSet s -> Int -> ()
validateKeyDIS name DenseIntSet {..} k
  | debug && not (inRange (0, capacityDIS - 1) k) = error $ name ++ ": out of range (" ++ show capacityDIS ++ "): " ++ show k
  | otherwise = ()

-- | \(O(1)\) Returns the number of elements in the set.
sizeDIS :: (PrimMonad m) => DenseIntSet (PrimState m) -> m Int
sizeDIS = (`UM.unsafeRead` 0) . sizeDIS_

-- | \(O(\log N)\) Tests if @k@ is in the set.
memberDIS :: (PrimMonad m) => DenseIntSet (PrimState m) -> Int -> m Bool
memberDIS is@DenseIntSet {..} k = do
  let (!q, !r) = k `divMod` wordDIS
  (`testBit` r) <$> GM.unsafeRead (G.unsafeHead vecDIS) q
  where
    !_ = validateKeyDIS "memberDIS" is k

-- | \(O(\log N)\) Tests if @k@ is not in the set.
notMemberDIS :: (PrimMonad m) => DenseIntSet (PrimState m) -> Int -> m Bool
notMemberDIS dis k = not <$> memberDIS dis k

-- | \(O(\log N)\) Inserts @k@ to the set.
insertDIS :: (PrimMonad m) => DenseIntSet (PrimState m) -> Int -> m ()
insertDIS is@DenseIntSet {..} k = do
  unlessM (memberDIS is k) $ do
    UM.unsafeModify sizeDIS_ (+ 1) 0
    V.foldM'_
      ( \i vec -> do
          let (!q, !r) = i `divMod` wordDIS
          GM.unsafeModify vec (`setBit` r) q
          return q
      )
      k
      vecDIS
  where
    !_ = validateKeyDIS "insertDIS" is k

-- | \(O(\log N)\) Deletes @k@ from the set.
deleteDIS :: (PrimMonad m) => DenseIntSet (PrimState m) -> Int -> m ()
deleteDIS is@DenseIntSet {..} k = do
  whenM (memberDIS is k) $ do
    UM.unsafeModify sizeDIS_ (subtract 1) 0
    V.foldM'_
      ( \(!b, !i) vec -> do
          let (!q, !r) = i `divMod` wordDIS
          -- TODO: early return is possible
          unless b $ do
            GM.unsafeModify vec (`clearBit` r) q
          -- `b` remembers if any other bit was on
          b' <- (/= 0) <$> GM.unsafeRead vec q
          return (b', q)
      )
      (False, k)
      vecDIS
  where
    !_ = validateKeyDIS "deleteDIS" is k

-- * GT / GE

-- | \(O(\log N)\) Finds the smallest @k'@ s.t. @k' >= k@ in the set.
lookupGEDIS :: (PrimMonad m) => DenseIntSet (PrimState m) -> Int -> m (Maybe Int)
lookupGEDIS DenseIntSet {..} = inner 0
  where
    inner h i
      | h >= V.length vecDIS = return Nothing
      -- ?
      | q == UM.length (G.unsafeIndex vecDIS h) = return Nothing
      | otherwise = do
          d <- (.>>. r) <$> GM.unsafeRead (G.unsafeIndex vecDIS h) q
          if d == 0
            then inner (h + 1) (q + 1)
            else
              Just
                <$> V.foldM'
                  ( \acc vec -> do
                      !dx <- lsbOf <$> GM.unsafeRead vec acc
                      return $ acc * wordDIS + dx
                  )
                  (i + lsbOf d)
                  (V.unsafeBackpermute vecDIS (V.enumFromStepN (h - 1) (-1) h))
      where
        (!q, !r) = i `divMod` wordDIS

-- | \(O(\log N)\)
findGEDIS :: (PrimMonad m) => DenseIntSet (PrimState m) -> Int -> m Int
findGEDIS is k = fromMaybe err <$> lookupGEDIS is k
  where
    err = error $ "findGEDIS: no element >= " ++ show k

-- | \(O(\log N)\)
lookupGTDIS :: (PrimMonad m) => DenseIntSet (PrimState m) -> Int -> m (Maybe Int)
lookupGTDIS is k = lookupGEDIS is (k + 1)

-- | \(O(\log N)\)
findGTDIS :: (PrimMonad m) => DenseIntSet (PrimState m) -> Int -> m Int
findGTDIS is k = findGEDIS is (k + 1)

-- * LT / LE

-- | \(O(\log N)\) Finds the biggest @k'@ s.t. @k' <= k@ in the set.
lookupLEDIS :: (PrimMonad m) => DenseIntSet (PrimState m) -> Int -> m (Maybe Int)
lookupLEDIS DenseIntSet {..} = inner 0
  where
    inner h i
      | h >= V.length vecDIS = return Nothing
      | i == -1 = return Nothing
      | otherwise = do
          d <- (.<<. (63 - r)) <$> GM.unsafeRead (G.unsafeIndex vecDIS h) q
          if d == 0
            then inner (h + 1) (q - 1)
            else do
              Just
                <$> V.foldM'
                  ( \acc vec -> do
                      !dx <- msbOf <$> GM.unsafeRead vec acc
                      return $ acc * wordDIS + dx
                  )
                  (i - countLeadingZeros d)
                  (V.unsafeBackpermute vecDIS (V.enumFromStepN (h - 1) (-1) h))
      where
        (!q, !r) = i `divMod` wordDIS

-- | \(O(\log N)\)
findLEDIS :: (PrimMonad m) => DenseIntSet (PrimState m) -> Int -> m Int
findLEDIS is k = fromMaybe err <$> lookupLEDIS is k
  where
    err = error $ "findLEDIS: no element <= " ++ show k

-- | \(O(\log N)\)
lookupLTDIS :: (PrimMonad m) => DenseIntSet (PrimState m) -> Int -> m (Maybe Int)
lookupLTDIS is k = lookupLEDIS is (k - 1)

-- | \(O(\log N)\)
findLTDIS :: (PrimMonad m) => DenseIntSet (PrimState m) -> Int -> m Int
findLTDIS is k = findLEDIS is (k - 1)

-- * Min / Max

-- | \(O(\log N)\) Not tested.
lookupMinDIS :: (PrimMonad m) => DenseIntSet (PrimState m) -> m (Maybe Int)
lookupMinDIS is = lookupGEDIS is 0

-- | \(O(\log N)\) Not tested.
findMinDIS :: (HasCallStack, PrimMonad m) => DenseIntSet (PrimState m) -> m Int
findMinDIS is = fromMaybe err <$> lookupMinDIS is
  where
    err = error "findMinDIS: not such a value"

-- | \(O(\log N)\) Not tested.
deleteFindMinMayDIS :: (PrimMonad m) => DenseIntSet (PrimState m) -> m (Maybe Int)
deleteFindMinMayDIS is = do
  lookupMinDIS is
    >>= mapM
      ( \key -> do
          deleteDIS is key
          return key
      )

-- | \(O(\log N)\) Not tested.
deleteFindMinDIS :: (HasCallStack, PrimMonad m) => DenseIntSet (PrimState m) -> m Int
deleteFindMinDIS is = do
  key <- findMinDIS is
  deleteDIS is key
  return key

-- | \(O(\log N)\) Not tested.
lookupMaxDIS :: (PrimMonad m) => DenseIntSet (PrimState m) -> m (Maybe Int)
lookupMaxDIS is = lookupLEDIS is (capacityDIS is - 1)

-- | \(O(\log N)\) Not tested.
findMaxDIS :: (HasCallStack, PrimMonad m) => DenseIntSet (PrimState m) -> m Int
findMaxDIS is = fromMaybe err <$> lookupMaxDIS is
  where
    err = error "findMaxDIS: not such a value"

-- | \(O(\log N)\) Not tested.
deleteFindMaxMayDIS :: (PrimMonad m) => DenseIntSet (PrimState m) -> m (Maybe Int)
deleteFindMaxMayDIS is = do
  lookupMinDIS is
    >>= mapM
      ( \key -> do
          deleteDIS is key
          return key
      )

-- | \(O(\log N)\) Not tested.
deleteFindMaxDIS :: (HasCallStack, PrimMonad m) => DenseIntSet (PrimState m) -> m Int
deleteFindMaxDIS is = do
  key <- findMaxDIS is
  deleteDIS is key
  return key

-- | \(O(N)\) Not tested.
unsafeKeysDIS :: (PrimMonad m) => DenseIntSet (PrimState m) -> m (U.Vector Int)
unsafeKeysDIS is = do
  vec <- U.unsafeFreeze (V.head (vecDIS is))
  return
    . U.filter
      ( \i ->
          let (!q, !r) = i `divMod` wordDIS
           in testBit (vec U.! q) r
      )
    $ U.generate (wordDIS * U.length vec) id
