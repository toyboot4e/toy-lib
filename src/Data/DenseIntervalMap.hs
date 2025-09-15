{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | @DenseIntervalMap@ is a dense map that manages non-overlapping @(l, r, x)@ value pairs.
--
-- = Typical problems
-- - [PAST 06 M - 等しい数](https://atcoder.jp/contests/past202104-open/tasks/past202104_m)
-- - [ABC 380 E - 1D Bucket Pool](https://atcoder.jp/contests/abc380/tasks/abc380_e)
module Data.DenseIntervalMap where

import Control.Monad (foldM_)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.DenseIntMap
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import GHC.Stack (HasCallStack)

-- | @DenseIntervalMap@ is a sparse map that manages non-overlapping @(l, r, x)@ value pairs.
newtype DenseIntervalMap s a = DenseIntervalMap
  { -- | @l@ -> @(r, a)@
    unDM :: DenseIntMap s (Int, a)
  }

-- | \(O(1)\) Creates an empty `DenseIntervalMap`.
newDM :: (PrimMonad m, U.Unbox a) => Int -> m (DenseIntervalMap (PrimState m) a)
newDM = fmap DenseIntervalMap . newDIM

-- | \(O(NW)\) Creates an interval map combining successive equal values into one.
fromVecMDM :: (PrimMonad m, Eq a, U.Unbox a) => U.Vector a -> (Int -> Int -> a -> m ()) -> m (DenseIntervalMap (PrimState m) a)
fromVecMDM xs onAdd = do
  dim <- newDIM (G.length xs)
  foldM_ (step dim) (0 :: Int) $ G.group xs
  pure $ DenseIntervalMap dim
  where
    step dim !l !xs' = do
      let !l' = l + G.length xs'
      insertDIM dim l (l' - 1, G.head xs')
      onAdd l (l' - 1) (G.head xs')
      pure l'

-- | \(O(NW)\) Pure variant of `fromVecMDM`
fromVecDM :: (PrimMonad m, Eq a, U.Unbox a) => U.Vector a -> m (DenseIntervalMap (PrimState m) a)
fromVecDM xs = fromVecMDM xs onAdd
  where
    onAdd _ _ _ = pure ()

-- | \(O(\min(n, W))\) Looks up an interval that contains @[l, r]@.
lookupDM :: (PrimMonad m, U.Unbox a) => DenseIntervalMap (PrimState m) a -> Int -> Int -> m (Maybe (Int, Int, a))
lookupDM (DenseIntervalMap dim) l r
  | r < l = pure Nothing
  | otherwise = do
      res <- lookupLEDIM dim l
      pure $ case res of
        Just (!l', (!r', !a))
          | r <= r' -> Just (l', r', a)
        _ -> Nothing

-- | \(O(\min(n, W))\) Looks up an interval that contains @[l, r]@ reads out the value.
readMayDM :: (PrimMonad m, U.Unbox a) => DenseIntervalMap (PrimState m) a -> Int -> Int -> m (Maybe a)
readMayDM (DenseIntervalMap dim) l r
  | r < l = pure Nothing
  | otherwise = do
      res <- lookupLEDIM dim l
      pure $ case res of
        Just (!_, (!r', !a))
          | r <= r' -> Just a
        _ -> Nothing

-- | \(O(\min(n, W))\) Looks up an interval that contains @[l, r]@ reads out the value.
readDM :: (HasCallStack, PrimMonad m, U.Unbox a) => DenseIntervalMap (PrimState m) a -> Int -> Int -> m a
readDM dm l r = do
  res <- readMayDM dm l r
  pure $ case res of
    Just !a -> a
    Nothing -> error $ "[readDM] not a member: " ++ show (l, r)

-- | \(O(\min(n, W))\) Shorthand for writing to an interval that contains @[l, r])@.
writeMDM :: (PrimMonad m, Eq a, U.Unbox a) => DenseIntervalMap (PrimState m) a -> Int -> Int -> a -> (Int -> Int -> a -> m ()) -> (Int -> Int -> a -> m ()) -> m ()
writeMDM dm l r x onAdd onDel = do
  res <- lookupDM dm l r
  case res of
    Just (!l', !r', !_) -> insertMDM dm l' r' x onAdd onDel
    Nothing -> pure ()

-- | \(O(\min(n, W))\) Boolean variant of `lookupDM`.
intersectsDM :: (PrimMonad m, U.Unbox a) => DenseIntervalMap (PrimState m) a -> Int -> Int -> m Bool
intersectsDM (DenseIntervalMap dim) l r
  | r < l = pure False
  | otherwise = do
      res <- lookupLEDIM dim l
      pure $ case res of
        Just (!_, (!r', !_)) -> r <= r'
        _ -> False

-- | \(O(\min(n, W))\) Point variant of `intersectsDM`.
containsDM :: (PrimMonad m, U.Unbox a) => DenseIntervalMap (PrimState m) a -> Int -> m Bool
containsDM dm i = intersectsDM dm i i

-- | Amortized \(O(\min(\log n, W))\) interval insertion with side effects. Old overlapping
-- intervals are overwritten.
insertMDM :: (PrimMonad m, Eq a, U.Unbox a) => DenseIntervalMap (PrimState m) a -> Int -> Int -> a -> (Int -> Int -> a -> m ()) -> (Int -> Int -> a -> m ()) -> m ()
insertMDM (DenseIntervalMap dim) l0 r0 x onAdd onDel = do
  !r <- handleRight l0 r0
  (!l', !r') <- handleLeft l0 r
  onAdd l' r' x
  insertDIM dim l' (r', x)
  pure ()
  where
    handleRight l r = do
      res <- lookupGEDIM dim l
      case res of
        Just interval0@(!_, (!_, !_)) -> run interval0 l r
        Nothing -> pure r

    -- Looks into intervals with @l' >= l0@.
    --           [----]
    -- (i)            *--------]   overwrite if it's x
    -- (ii)   [-------]*      delete anyways
    -- (iii)    *(------]     overwrite if it's x, or
    run (!l', (!r', !x')) l r
      | l' > r + 1 = do
          -- not adjacent: end.
          pure r
      -- (i)
      | l' == r + 1 && x' == x = do
          -- adjacent interval with the same value: merge into one.
          onDel (r + 1) r' x'
          deleteDIM dim l'
          pure r'
      | l' == r + 1 = do
          -- adjacent interval with different values: nothing to do.
          pure r
      -- (ii)
      | r' <= r = do
          -- inside the interval: delete and continue
          onDel l' r' x'
          deleteDIM dim l'
          -- TODO: wrap it (DRY)
          res <- lookupGTDIM dim l'
          case res of
            Just rng -> run rng l r
            Nothing -> pure r
      -- (iii)
      | x' == x = do
          -- intersecting interval with the same value: merge into one.
          onDel l' r' x'
          deleteDIM dim l'
          pure r'
      | otherwise = do
          -- intersecting interval with a different value: delete the intersection.
          onDel l' r x'
          deleteDIM dim l'
          insertDIM dim (r + 1) (r', x')
          pure r

    handleLeft l r = do
      res <- lookupLTDIM dim l
      case res of
        Nothing -> pure (l, r)
        Just (!l', (!r', !x'))
          -- (i): adjacent interval
          | r' + 1 == l0 && x' == x -> do
              -- adjacent interval with the same value: merge into one.
              onDel l' r' x'
              deleteDIM dim l'
              pure (l', r)
          | r' + 1 == l -> do
              -- adjacent interval with different values: nothing to do.
              pure (l, r)
          -- (ii): not intersecting
          | r' < l -> do
              pure (l, r)
          -- (iii): intersecting
          | x' == x -> do
              -- insersecting interval with the same value: merge into one.
              onDel l' r' x'
              deleteDIM dim l'
              pure (min l l', max r r')
          | r' > r -> do
              -- intersecting interval with a different value: split into three.
              onDel l' r' x'
              onAdd l' (l - 1) x'
              onAdd (r + 1) r' x'
              deleteDIM dim l'
              insertDIM dim l' (l - 1, x')
              insertDIM dim (r + 1) (r', x')
              pure (l, r)
          | otherwise -> do
              -- insersecting interval with a different value: delete.
              onDel l r' x'
              deleteDIM dim l'
              insertDIM dim l' (l - 1, x')
              pure (l, r)

-- | Amortized \(O(\min(\log n, W))\) interval insertion. Old overlapping intervals are overwritten.
insertDM :: (PrimMonad m, Eq a, U.Unbox a) => DenseIntervalMap (PrimState m) a -> Int -> Int -> a -> m ()
insertDM dm l r x = insertMDM dm l r x onAdd onDel
  where
    onAdd _ _ _ = pure ()
    onDel _ _ _ = pure ()

-- | Amortized \(O(\min(\log n, W))\) interval deletion with side effects.
deleteMDM :: (PrimMonad m, U.Unbox a) => DenseIntervalMap (PrimState m) a -> Int -> Int -> (Int -> Int -> a -> m ()) -> m ()
deleteMDM (DenseIntervalMap dim) l0 r0 onDel = do
  r <- handleRight l0 r0
  handleLeft l0 r
  pure ()
  where
    handleRight l r = do
      res <- lookupGEDIM dim l
      case res of
        Just interval0@(!_, (!_, !_)) -> run interval0 l r
        Nothing -> pure r

    run (!l', (!r', !x')) l r
      | l' >= r + 1 = do
          pure r
      | r' <= r = do
          onDel l' r' x'
          deleteDIM dim l'
          res <- lookupGTDIM dim l'
          case res of
            Just rng -> run rng l r
            Nothing -> pure r
      | otherwise = do
          onDel l' r x'
          deleteDIM dim l'
          insertDIM dim (r + 1) (r', x')
          pure r

    handleLeft l r = do
      res <- lookupLTDIM dim l
      case res of
        Nothing -> pure ()
        Just (!l', (!r', !x'))
          | r' < l -> do
              pure ()
          | r' > r -> do
              onDel l' r' x'
              -- REMARK: this deletion is redundant
              -- deleteDIM dim l'
              insertDIM dim l' (l - 1, x')
              insertDIM dim (r + 1) (r', x')
              pure ()
          | otherwise -> do
              onDel l r' x'
              -- TODO: delete is redundant?
              deleteDIM dim l'
              insertDIM dim l' (l - 1, x')
              pure ()

-- | Amortized \(O(\min(\log n, W))\) interval deletion.
deleteDM :: (PrimMonad m, U.Unbox a) => DenseIntervalMap (PrimState m) a -> Int -> Int -> m ()
deleteDM dm l r = deleteMDM dm l r onDel
  where
    onDel _ _ _ = pure ()

-- | \(O(\min(\log n, W))\) Mex retrieval. REMARK: The interval map has to be like a set. Use
-- @maxIS@ when possible.
mexDM :: (PrimMonad m, U.Unbox a) => DenseIntervalMap (PrimState m) a -> m Int
mexDM (DenseIntervalMap dim) = do
  res <- lookupLEDIM dim 0
  pure $ case res of
    Just (!l', (!r', !_))
      | l' == 0 -> r' + 1
    Nothing -> 0
