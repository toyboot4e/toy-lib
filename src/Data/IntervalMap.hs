{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | @IntervalMap@ is a sparse map that manages non-overlapping @(l, r, x)@ value pairs.
--
-- Typically used with @StateT@. Try @gets@, @puts@ and @modifyM@.
--
-- = Typical problems
-- - [PAST 06 M - 等しい数](https://atcoder.jp/contests/past202104-open/tasks/past202104_m)
-- - [ABC 380 E - 1D Bucket Pool](https://atcoder.jp/contests/abc380/tasks/abc380_e)
module Data.IntervalMap where

import Control.Monad (foldM)
import Control.Monad.Identity (runIdentity)
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Generic as G
import GHC.Stack (HasCallStack)

-- | @IntervalMap@ is a sparse map that manages non-overlapping @(l, r, x)@ value pairs.
newtype IntervalMap a = IntervalMap
  { -- | @l@ -> @(r, a)@
    unIM :: IM.IntMap (Int, a)
  }
  deriving newtype (Show, Eq)

-- | \(O(1)\) Creates an empty `IntervalMap`.
emptyIM :: IntervalMap a
emptyIM = IntervalMap IM.empty

-- | \(O(NW)\) Creates an interval map combining successive equal values into one.
fromVecMIM :: (G.Vector v a, Eq a, Monad m) => v a -> (Int -> Int -> a -> m ()) -> m (IntervalMap a)
fromVecMIM xs onAdd = fmap (IntervalMap . fst) $ foldM step (IM.empty, 0 :: Int) $ G.group xs
  where
    step (!map, !l) !xs' = do
      let !l' = l + G.length xs'
          !map' = IM.insert l (l' - 1, G.head xs') map
      onAdd l (l' - 1) (G.head xs')
      return (map', l')

-- | \(O(NW)\) Pure variant of `fromVecMIM`
fromVecIM :: (G.Vector v a, Eq a) => v a -> IntervalMap a
fromVecIM xs = runIdentity (fromVecMIM xs onAdd)
  where
    onAdd _ _ _ = pure ()

-- | \(O(\min(n, W))\) Looks up an interval that contains @[l, r]@.
lookupIM :: Int -> Int -> IntervalMap a -> Maybe (Int, Int, a)
lookupIM l r (IntervalMap map)
  | r < l = Nothing
  | otherwise = case IM.lookupLE l map of
      Just (!l', (!r', !a))
        | r <= r' -> Just (l', r', a)
      _ -> Nothing

-- | \(O(\min(n, W))\) Looks up an interval that contains @[l, r]@ reads out the value.
readMayIM :: Int -> Int -> IntervalMap a -> Maybe a
readMayIM l r (IntervalMap map)
  | r < l = Nothing
  | otherwise = case IM.lookupLE l map of
      Just (!_, (!r', !a))
        | r <= r' -> Just a
      _ -> Nothing

-- | \(O(\min(n, W))\) Looks up an interval that contains @[l, r]@ reads out the value.
readIM :: (HasCallStack) => Int -> Int -> IntervalMap a -> a
readIM l r rm = case readMayIM l r rm of
  Nothing -> error $ "[readIM] not a member: " ++ show (l, r)
  Just !a -> a

-- | \(O(\min(n, W))\) Shorthand for writing to an interval that contains @[l, r])@.
writeMIM :: (Monad m, Eq a) => Int -> Int -> a -> (Int -> Int -> a -> m ()) -> (Int -> Int -> a -> m ()) -> IntervalMap a -> m (IntervalMap a)
writeMIM l r x onAdd onDel map = case lookupIM l r map of
  Just (!l', !r', !_) -> insertMIM l' r' x onAdd onDel map
  Nothing -> return map

-- | \(O(\min(n, W))\) Boolean variant of `lookupIM`.
intersectsIM :: Int -> Int -> IntervalMap a -> Bool
intersectsIM l r (IntervalMap map)
  | r < l = False
  | otherwise = case IM.lookupLE l map of
      Just (!_, (!r', !_)) -> r <= r'
      _ -> False

-- | \(O(\min(n, W))\) Point variant of `intersectsIM`.
containsIM :: Int -> IntervalMap a -> Bool
containsIM i = intersectsIM i i

-- | Amortized \(O(\min(\log n, W))\) interval insertion with side effects. Old overlapping
-- intervals are overwritten.
insertMIM :: (Monad m, Eq a) => Int -> Int -> a -> (Int -> Int -> a -> m ()) -> (Int -> Int -> a -> m ()) -> IntervalMap a -> m (IntervalMap a)
insertMIM l0 r0 x onAdd onDel (IntervalMap map0) = do
  (!r, !map) <- handleRight l0 r0 map0
  (!l', !r', !map') <- handleLeft l0 r map
  onAdd l' r' x
  let !map'' = IM.insert l' (r', x) map'
  return $! IntervalMap map''
  where
    handleRight l r map = case IM.lookupGE l map of
      Just interval0@(!_, (!_, !_)) -> run interval0 l r map
      Nothing -> return (r, map)

    -- Looks into intervals with @l' >= l0@.
    --           [----]
    -- (i)            *--------]   overwrite if it's x
    -- (ii)   [-------]*      delete anyways
    -- (iii)    *(------]     overwrite if it's x, or
    run (!l', (!r', !x')) l r map
      | l' > r + 1 = do
          -- not adjacent: end.
          return (r, map)
      -- (i)
      | l' == r + 1 && x' == x = do
          -- adjacent interval with the same value: merge into one.
          onDel (r + 1) r' x'
          let !map' = IM.delete l' map
          return (r', map')
      | l' == r + 1 = do
          -- adjacent interval with different values: nothing to do.
          return (r, map)
      -- (ii)
      | r' <= r = do
          -- inside the interval: delete and continue
          onDel l' r' x'
          let !map' = IM.delete l' map
          -- TODO: wrap it (DRY)
          case IM.lookupGT l' map' of
            Just rng -> run rng l r map'
            Nothing -> return (r, map')
      -- (iii)
      | x' == x = do
          -- intersecting interval with the same value: merge into one.
          onDel l' r' x'
          let !map' = IM.delete l' map
          return (r', map')
      | otherwise = do
          -- intersecting interval with a different value: delete the intersection.
          onDel l' r x'
          let !map' = IM.insert (r + 1) (r', x') $ IM.delete l' map
          return (r, map')

    handleLeft l r map = case IM.lookupLT l map of
      Nothing -> return (l, r, map)
      Just (!l', (!r', !x'))
        -- (i): adjacent interval
        | r' + 1 == l0 && x' == x -> do
            -- adjacent interval with the same value: merge into one.
            onDel l' r' x'
            let !map' = IM.delete l' map
            return (l', r, map')
        | r' + 1 == l -> do
            -- adjacent interval with different values: nothing to do.
            return (l, r, map)
        -- (ii): not intersecting
        | r' < l -> do
            return (l, r, map)
        -- (iii): intersecting
        | x' == x -> do
            -- insersecting interval with the same value: merge into one.
            onDel l' r' x'
            let !map' = IM.delete l' map
            return (min l l', max r r', map')
        | r' > r -> do
            -- intersecting interval with a different value: split into three.
            onDel l' r' x'
            onAdd l' (l - 1) x'
            onAdd (r + 1) r' x'
            let !map' = IM.insert (r + 1) (r', x') $ IM.insert l' (l - 1, x') $ IM.delete l' map
            return (l, r, map')
        | otherwise -> do
            -- insersecting interval with a different value: delete.
            onDel l r' x'
            let !map' = IM.insert l' (l - 1, x') $ IM.delete l' map
            return (l, r, map')

-- | Amortized \(O(\min(\log n, W))\) interval insertion. Old overlapping intervals are overwritten.
insertIM :: (Eq a) => Int -> Int -> a -> IntervalMap a -> IntervalMap a
insertIM l r x rm = runIdentity (insertMIM l r x onAdd onDel rm)
  where
    onAdd _ _ _ = pure ()
    onDel _ _ _ = pure ()

-- | Amortized \(O(\min(\log n, W))\) interval deletion with side effects.
deleteMIM :: (Monad m) => Int -> Int -> (Int -> Int -> a -> m ()) -> IntervalMap a -> m (IntervalMap a)
deleteMIM l0 r0 onDel (IntervalMap map0) = do
  (!r, !map) <- handleRight l0 r0 map0
  !map' <- handleLeft l0 r map
  return $ IntervalMap map'
  where
    handleRight l r map = case IM.lookupGE l map of
      Just interval0@(!_, (!_, !_)) -> run interval0 l r map
      Nothing -> return (r, map)

    run (!l', (!r', !x')) l r map
      | l' >= r + 1 = do
          return (r, map)
      | r' <= r = do
          onDel l' r' x'
          let !map' = IM.delete l' map
          case IM.lookupGT l' map' of
            Just rng -> run rng l r map'
            Nothing -> return (r, map')
      | otherwise = do
          onDel l' r x'
          let !map' = IM.insert (r + 1) (r', x') $ IM.delete l' map
          return (r, map')

    handleLeft l r map = case IM.lookupLT l map of
      Nothing -> return map
      Just (!l', (!r', !x'))
        | r' < l -> do
            return map
        | r' > r -> do
            onDel l' r' x'
            -- REMARK: this deletion is redundant
            -- IM.delete l' map
            let !map' = IM.insert (r + 1) (r', x') $ IM.insert l' (l - 1, x') map
            return map'
        | otherwise -> do
            onDel l r' x'
            let !map' = IM.insert l' (l - 1, x') $ IM.delete l' map
            return map'

-- | Amortized \(O(\min(\log n, W))\) interval deletion.
deleteIM :: Int -> Int -> IntervalMap a -> IntervalMap a
deleteIM l r rm = runIdentity (deleteMIM l r onDel rm)
  where
    onDel _ _ _ = pure ()

-- | \(O(\min(\log n, W))\) Mex retrieval. REMARK: The interval map has to be like a set. Use
-- @maxIS@ when possible.
mexIM :: IntervalMap a -> Int
mexIM (IntervalMap map) = case IM.lookupLE 0 map of
  Just (!l', (!r', !_))
    | l' == 0 -> r' + 1
  Nothing -> 0
