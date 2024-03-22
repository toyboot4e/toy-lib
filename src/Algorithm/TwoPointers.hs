-- | \(O(f N)\) Two pointers method.
--
-- = Stateless two pointer method
--
-- The stateless two pointer method assumes you can make up an \(O(1)\) @Int -> Int -> Bool@
-- predicate. Maybe cummulative sum comes with it.
--
-- = Statefull two pointer method
--
-- See also: <https://zenn.dev/osushi0x/articles/e5bd9fe60abee4>
module Algorithm.TwoPointers where

import Control.Monad.Identity
import Data.List (unfoldr)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

-- | \(O(f N)\) Stateless two pointer method over a range. Returns the longest non-null inclusive
-- ranges for each @l@ that satisfy the given @check@.
twoPointers :: Int -> (Int -> Int -> Bool) -> [(Int, Int)]
twoPointers !n !p = unfoldr (uncurry f) s0
  where
    -- the inners are the same as @twoPointersU@:
    !s0 = (0, 0) :: (Int, Int)
    f l r
      | l == n = Nothing
      | not (p l r) = f (l + 1) (max (l + 1) r)
      | otherwise = Just ((l, r'), (l + 1, max (l + 1) r'))
      where
        -- run peek check and advance on success
        r' = until ((||) <$> (== n - 1) <*> not . p l . succ) succ r

-- | \(O(f N)\) Stateless two pointer method over a range. Returns the longest non-null inclusive
-- ranges for each @l@ that satisfy the given @check@.
twoPointersU :: Int -> (Int -> Int -> Bool) -> U.Vector (Int, Int)
twoPointersU !n !p = U.unfoldr (uncurry f) s0
  where
    !s0 = (0, 0) :: (Int, Int)
    f l r
      | l == n = Nothing
      | not (p l r) = f (l + 1) (max (l + 1) r)
      | otherwise = Just ((l, r'), (l + 1, max (l + 1) r'))
      where
        -- run peek check and advance on success
        r' = until ((||) <$> (== n - 1) <*> not . p l . succ) succ r

-- | \(O(f N)\) Stateful two pointer method over a vector. Returns the longest non-null inclusive
-- ranges for each @l@ that satisfy the given @check@.
{-# INLINE twoPtrM #-}
twoPtrM :: forall acc m v a. (Monad m, G.Vector v a) => acc -> (acc -> a -> m Bool) -> (acc -> a -> m acc) -> (acc -> a -> m acc) -> v a -> m [(Int, Int)]
twoPtrM acc0 p onNext onPop xs0 = inner acc0 xs0 xs0 (0 :: Int) (0 :: Int)
  where
    inner :: acc -> v a -> v a -> Int -> Int -> m [(Int, Int)]
    inner acc pops nexts l r = case G.uncons pops of
      Nothing -> return []
      Just (!y, !pops') -> case G.uncons nexts of
        Just (!x, !nexts') -> do
          b <- (r - l == 0 ||) <$> p acc x
          if b
            then do
              !acc' <- onNext acc x
              inner acc' pops nexts' l (r + 1)
            else do
              !acc' <- onPop acc y
              ((l, r) :) <$> inner acc' pops' nexts (l + 1) r
        -- FIXME: same code twice..
        Nothing -> do
          !acc' <- onPop acc y
          ((l, r) :) <$> inner acc' pops' nexts (l + 1) r

-- TODO: purely mutation only two pointer method

-- | \(O(f N)\)
{-# INLINE twoPtr #-}
twoPtr :: (G.Vector v a) => acc -> (acc -> a -> Bool) -> (acc -> a -> acc) -> (acc -> a -> acc) -> v a -> [(Int, Int)]
twoPtr acc0 p onNext onPop = runIdentity . twoPtrM acc0 ((pure .) . p) ((pure .) . onNext) ((pure .) . onPop)

-- TODO: try also the `Writer`
