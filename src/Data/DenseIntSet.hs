{-# LANGUAGE RecordWildCards #-}

-- | Dense int set or a 64-ary tree that covers @[0, n)@.
--
-- <https://github.com/maspypy/library>
module Data.DenseIntSet where

import Control.Monad
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bifunctor (bimap)
import Data.Bits
import Data.Ix
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Stack (HasCallStack)
import Math.BitSet (lsbOf, msbOf)

-- | Dense int set or a W-ary tree where @W = wordDIS@.
wordDIS :: Int
wordDIS = 64

-- | Dense int set or a 64-ary tree that covers @[0, n)@.
--
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
  { -- | The number of elements.
    capacityDIS :: !Int,
    -- TODO: track the number of elements int the set

    -- | Smallest \(i\) s.t. \(\mathcal{wordDIS}^{i} \ge \mathcal{capacityDIS}\). Length of @vecDIS@.
    logSizeDIS :: !Int,
    -- | Segments.
    vecDIS :: !(V.Vector (UM.MVector s Int))
  }

-- | \(O(N \log N)\) Creates a new `DenseIntSet` that covers @[0, n)@.
newDIS :: (PrimMonad m) => Int -> m (DenseIntSet (PrimState m))
newDIS capacityDIS = do
  vecDIS <-
    V.unfoldrExactNM
      (max 1 logSizeDIS)
      ( \len -> do
          let !len' = (len + wordDIS - 1) `div` wordDIS
          (,len') <$> UM.replicate len' 0
      )
      capacityDIS
  return DenseIntSet {..}
  where
    (!_, !logSizeDIS) =
      until
        ((<= 1) . fst)
        (bimap ((`div` wordDIS) . (+ (wordDIS - 1))) (+ 1))
        (capacityDIS, 0)

-- buildDIS :: (PrimMonad m) => Int -> (Int -> Int) -> m (DenseIntSet (PrimState m))
-- buildDIS capacityDIS f = runST $ do

-- | (Internal)
validateKeyDIS :: (HasCallStack) => String -> DenseIntSet s -> Int -> ()
validateKeyDIS name DenseIntSet {..} k
  | not (inRange (0, capacityDIS - 1) k) = error $ name ++ ": out of range (" ++ show capacityDIS ++ "): " ++ show k
  | otherwise = ()

-- | \(O(\log N)\) Tests if @k@ is in the set.
memberDIS :: (PrimMonad m) => DenseIntSet (PrimState m) -> Int -> m Bool
memberDIS is@DenseIntSet {..} k = do
  let (!q, !r) = k `divMod` wordDIS
  (`testBit` r) <$> GM.read (G.head vecDIS) q
  where
    !_ = validateKeyDIS "memberDIS" is k

-- | \(O(\log N)\) Inserts @k@ to the set.
insertDIS :: (PrimMonad m) => DenseIntSet (PrimState m) -> Int -> m ()
insertDIS is@DenseIntSet {..} k = do
  V.foldM'_
    ( \i vec -> do
        let (!q, !r) = i `divMod` wordDIS
        GM.modify vec (`setBit` r) q
        return q
    )
    k
    vecDIS
  where
    !_ = validateKeyDIS "insertDIS" is k

-- | \(O(\log N)\) Deletes @k@ from the set.
deleteDIS :: (PrimMonad m) => DenseIntSet (PrimState m) -> Int -> m ()
deleteDIS is@DenseIntSet {..} k = do
  V.foldM'_
    ( \(!b, !i) vec -> do
        let (!q, !r) = i `divMod` wordDIS
        -- TODO: early return is possible
        unless b $ do
          GM.modify vec (`clearBit` r) q
        -- `b` remembers if any other bit was on
        b' <- (/= 0) <$> GM.read vec q
        return (b', q)
    )
    (False, k)
    vecDIS
  where
    !_ = validateKeyDIS "deleteDIS" is k

-- | \(O(\log N)\) Finds the smallest @k'@ s.t. @k' >= k@ in the set.
lookupGEDIS :: (PrimMonad m) => DenseIntSet (PrimState m) -> Int -> m (Maybe Int)
lookupGEDIS DenseIntSet {..} = inner 0
  where
    inner h i
      | h >= V.length vecDIS = return Nothing
      -- ?
      | q == UM.length (vecDIS G.! h) = return Nothing
      | otherwise = do
          d <- (.>>. r) <$> GM.read (vecDIS G.! h) q
          if d == 0
            then inner (h + 1) (q + 1)
            else
              Just
                <$> V.foldM'
                  ( \acc vec -> do
                      !dx <- lsbOf <$> GM.read vec acc
                      return $ acc * wordDIS + dx
                  )
                  (i + lsbOf d)
                  (V.reverse (V.take h vecDIS))
      where
        (!q, !r) = i `divMod` wordDIS

-- | \(O(\log N)\)
findGEDIS :: (PrimMonad m) => DenseIntSet (PrimState m) -> Int -> m Int
findGEDIS fs k = fromMaybe err <$> lookupGEDIS fs k
  where
    err = error $ "findGEDIS: no element >= " ++ show k

-- | \(O(\log N)\)
lookupGTDIS :: (PrimMonad m) => DenseIntSet (PrimState m) -> Int -> m (Maybe Int)
lookupGTDIS fs k = lookupGEDIS fs (k + 1)

-- | \(O(\log N)\)
findGTDIS :: (PrimMonad m) => DenseIntSet (PrimState m) -> Int -> m Int
findGTDIS fs k = findGEDIS fs (k + 1)

-- | \(O(\log N)\) Finds the biggest @k'@ s.t. @k' <= k@ in the set.
lookupLEDIS :: (PrimMonad m) => DenseIntSet (PrimState m) -> Int -> m (Maybe Int)
lookupLEDIS DenseIntSet {..} = inner 0
  where
    inner h i
      | h >= V.length vecDIS = return Nothing
      | i == -1 = return Nothing
      | otherwise = do
          d <- (.<<. (63 - r)) <$> GM.read (vecDIS G.! h) q
          if d == 0
            then inner (h + 1) (q - 1)
            else do
              Just
                <$> V.foldM'
                  ( \acc vec -> do
                      !dx <- msbOf <$> GM.read vec acc
                      return $ acc * wordDIS + dx
                  )
                  (i - countLeadingZeros d)
                  (V.reverse (V.take h vecDIS))
      where
        (!q, !r) = i `divMod` wordDIS

-- | \(O(\log N)\)
findLEDIS :: (PrimMonad m) => DenseIntSet (PrimState m) -> Int -> m Int
findLEDIS fs k = fromMaybe err <$> lookupLEDIS fs k
  where
    err = error $ "findLEDIS: no element <= " ++ show k

-- | \(O(\log N)\)
lookupLTDIS :: (PrimMonad m) => DenseIntSet (PrimState m) -> Int -> m (Maybe Int)
lookupLTDIS fs k = lookupLEDIS fs (k - 1)

-- | \(O(\log N)\)
findLTDIS :: (PrimMonad m) => DenseIntSet (PrimState m) -> Int -> m Int
findLTDIS fs k = findLEDIS fs (k - 1)
