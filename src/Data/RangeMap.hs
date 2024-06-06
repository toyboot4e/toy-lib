{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Range map.
--
-- Typically used with @StateT@.
--
-- = Typical problems
-- - [PAST 06 M - 等しい数](https://atcoder.jp/contests/past202104-open/tasks/past202104_m)
module Data.RangeMap where

import Control.Monad (foldM)
import Control.Monad.Identity (runIdentity)
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Generic as G
import GHC.Stack (HasCallStack)

-- TODO: faster implementation
-- TODO: quickcheck (e.g., adjacent ranges have different values, compare it with naive vector-based solution)

newtype RangeMap a = RangeMap
  { -- | @l@ -> @(r, a)@
    unRM :: IM.IntMap (Int, a)
  }
  deriving newtype (Show, Eq)

emptyRM :: RangeMap a
emptyRM = RangeMap IM.empty

-- | Creates a range map combining successive equal values into one.
fromVecMRM :: (G.Vector v a, Eq a, Monad m) => v a -> (Int -> Int -> a -> m ()) -> m (RangeMap a)
fromVecMRM xs onAdd = fmap (RangeMap . fst) $ foldM step (IM.empty, 0 :: Int) $ G.group xs
  where
    step (!map, !l) !xs' = do
      let !l' = l + G.length xs'
          !map' = IM.insert l (l' - 1, G.head xs') map
      onAdd l (l' - 1) (G.head xs')
      return (map', l')

-- | Pure variant of `fromVecMRM`
fromVecRM :: (G.Vector v a, Eq a) => v a -> RangeMap a
fromVecRM xs = runIdentity (fromVecMRM xs onAdd)
  where
    onAdd _ _ _ = pure ()

-- | Looks up a range that contains @[l, r]@.
lookupRM :: Int -> Int -> RangeMap a -> Maybe (Int, Int, a)
lookupRM l r (RangeMap map)
  | r < l = Nothing
  | otherwise = case IM.lookupLE l map of
      Just (!l', (!r', !a))
        | r <= r' -> Just (l', r', a)
      _ -> Nothing

-- | Looks up a range that contains @[l, r]@ reads out the value.
readMayRM :: Int -> Int -> RangeMap a -> Maybe a
readMayRM l r (RangeMap map)
  | r < l = Nothing
  | otherwise = case IM.lookupLE l map of
      Just (!_, (!r', !a))
        | r <= r' -> Just a
      _ -> Nothing

-- | Looks up a range that contains @[l, r]@ reads out the value.
readRM :: (HasCallStack) => Int -> Int -> RangeMap a -> a
readRM l r rm = case readMayRM l r rm of
  Nothing -> error $ "[readRM] not a member: " ++ show (l, r)
  Just !a -> a

-- | Boolean variant of `lookupRM`.
intersectsRM :: Int -> Int -> RangeMap a -> Bool
intersectsRM l r (RangeMap map)
  | r < l = False
  | otherwise = case IM.lookupLE l map of
      Just (!_, (!r', !_)) -> r <= r'
      _ -> False

-- | Point variant of `intersectsRM`.
containsRM :: Int -> RangeMap a -> Bool
containsRM i = intersectsRM i i

-- TODO: deleteMRM

insertMRM :: (Monad m, Eq a) => Int -> Int -> a -> (Int -> Int -> a -> m ()) -> (Int -> Int -> a -> m ()) -> RangeMap a -> m (RangeMap a)
insertMRM l0 r0 x onAdd onDel (RangeMap map0) = do
  (!r, !map) <- handleRight l0 r0 map0
  (!l', !r', !map') <- handleLeft l0 r map
  onAdd l' r' x
  let !map'' = IM.insert l' (r', x) map'
  return $! RangeMap map''
  where
    handleRight l r map = case IM.lookupGE l map of
      Just range0@(!_, (!_, !_)) -> run range0 l r map
      Nothing -> return (r, map)

    -- Looks into ranges with @l' >= l0@.
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
          -- adjacent range with the same value: merge into one.
          onDel (r + 1) r' x'
          let !map' = IM.delete l' map
          return (r', map')
      | l' == r + 1 = do
          -- adjacent range with different values: nothing to do.
          return (r, map)
      -- (ii)
      | r' <= r = do
          -- inside the range: delete and continue
          onDel l' r' x'
          let !map' = IM.delete l' map
          -- TODO: wrap it (DRY)
          case IM.lookupGT l' map' of
            Just rng -> run rng l r map'
            Nothing -> return (r, map')
      -- (iii)
      | x' == x = do
          -- intersecting range with the same value: merge into one.
          onDel l' r' x'
          let !map' = IM.delete l' map
          return (r', map')
      | otherwise = do
          -- intersecting range with a different value: delete the intersection.
          onDel l' r x'
          let !map' = IM.insert (r + 1) (r', x') $ IM.delete l' map
          return (r, map')

    handleLeft l r map = case IM.lookupLT l map of
      Nothing -> return (l, r, map)
      Just (!l', (!r', !x'))
        -- (i): adjacent range
        | r' + 1 == l0 && x' == x -> do
            -- adjacent range with the same value: merge into one.
            onDel l' r' x'
            let !map' = IM.delete l' map
            return (l', r, map')
        | r' + 1 == l -> do
            -- adjacent range with different values: nothing to do.
            return (l, r, map)
        -- (ii): not intersecting
        | r' < l -> do
            return (l, r, map)
        -- (iii): intersecting
        | x' == x -> do
            -- insersecting range with the same value: merge into one.
            onDel l' r' x'
            let !map' = IM.delete l' map
            return (min l l', max r r', map')
        | r' > r -> do
            -- intersecting range with a different value: split into three.
            onDel l' r' x'
            onAdd l' (l - 1) x'
            onAdd (r + 1) r' x'
            let !map' = IM.insert (r + 1) (r', x') $ IM.insert l' (l - 1, x') $ IM.delete l' map
            return (l, r, map')
        | otherwise -> do
            -- insersecting range with a different value: delete.
            onDel l r' x'
            let !map' = IM.insert l' (l - 1, x') $ IM.delete l' map
            return (l, r, map')

-- | Pure variant of `insertMRM`.
insertRM :: (Eq a) => Int -> Int -> a -> RangeMap a -> RangeMap a
insertRM l r x rm = runIdentity (insertMRM l r x onAdd onDel rm)
  where
    onAdd _ _ _ = pure ()
    onDel _ _ _ = pure ()

deleteMRM :: (Monad m) => Int -> Int -> (Int -> Int -> a -> m ()) -> RangeMap a -> m (RangeMap a)
deleteMRM l0 r0 onDel (RangeMap map0) = do
  (!r, !map) <- handleRight l0 r0 map0
  !map' <- handleLeft l0 r map
  return $ RangeMap map'
  where
    handleRight l r map = case IM.lookupGE l map of
      Just range0@(!_, (!_, !_)) -> run range0 l r map
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
            let !map' = IM.insert (r + 1) (r', x') $ IM.insert l' (l - 1, x') $ IM.delete l' map
            return map'
        | otherwise -> do
            onDel l r' x'
            let !map' = IM.insert l' (l - 1, x') $ IM.delete l' map
            return map'

-- | Pure variant of `insertMRM`.
deleteRM :: Int -> Int -> RangeMap a -> RangeMap a
deleteRM l r rm = runIdentity (deleteMRM l r onDel rm)
  where
    onDel _ _ _ = pure ()

-- | REMARK: The range map has to be like a set. Use @maxRS@ when possible.
mexRM :: RangeMap a -> Int
mexRM (RangeMap map) = case IM.lookupLE 0 map of
  Just (!l', (!r', !_))
    | l' == 0 -> r' + 1
  Nothing -> 0
