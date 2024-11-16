module ToyLib.Prelude where

import Control.Monad.Fix
import Data.Bifunctor
import qualified Data.ByteString.Char8 as BS
import Data.Ix
import Data.Tuple.Extra hiding (first, second)
import Data.Utils.Unindex
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

-- | \(O(N)\)
{-# INLINE rleOf #-}
rleOf :: BS.ByteString -> [(Char, Int)]
rleOf = map (\s -> (BS.head s, BS.length s)) . BS.group

-- | \(O(N)\)
{-# INLINE rleOfU #-}
rleOfU :: BS.ByteString -> U.Vector (Char, Int)
-- fromList grows double by double though
rleOfU = U.fromList . rleOf

-- | \(O(1)\)
{-# INLINE square #-}
square :: (Num a) => a -> a
square !x = x * x

-- | \(O(1)\) Inaccurate, but fast `Int` square root.
-- TODO: Fast and accurate implementation
{-# INLINE isqrt #-}
isqrt :: Int -> Int
isqrt = round @Double . sqrt . fromIntegral

-- {-# INLINE modifyArray #-}
-- modifyArray :: (MArray a e m, Ix i) => a i e -> (e -> e) -> i -> m ()
-- modifyArray !ary !f !i = do
--   !v <- f <$> readArray ary i
--   writeArray ary i v

-- | \(O(1)\) Two-variable function compositon.
{-# INLINE (.:) #-}
(.:) :: (b -> c) -> (a1 -> a2 -> b) -> (a1 -> a2 -> c)
(.:) = (.) . (.)

-- | \(O(E)\) Edge duplication.
{-# INLINE swapDupeU #-}
swapDupeU :: U.Vector (Int, Int) -> U.Vector (Int, Int)
swapDupeU = U.concatMap (\(!u, !v) -> U.fromListN 2 [(u, v), (v, u)])

-- | \(O(E)\) Converts undirected edges into directed edges.
{-# INLINE swapDupeW #-}
swapDupeW :: (U.Unbox w) => U.Vector (Int, Int, w) -> U.Vector (Int, Int, w)
swapDupeW = U.concatMap (\(!u, !v, !d) -> U.fromListN 2 [(u, v, d), (v, u, d)])

-- TODO: NOINLINE + force makes more sense?

{-# INLINE ortho4 #-}
ortho4 :: U.Vector (Int, Int)
ortho4 = U.fromList [(0, 1), (0, -1), (1, 0), (-1, 0)]

{-# INLINE ortho4' #-}
ortho4' :: ((Int, Int), (Int, Int)) -> (Int, Int) -> U.Vector (Int, Int)
ortho4' bnd base = U.filter (inRange bnd) $ U.map (add2 base) ortho4

-- | Specifically for grids with generic search functions.
{-# INLINE orthoWith #-}
orthoWith :: ((Int, Int), (Int, Int)) -> ((Int, Int) -> Bool) -> (Int -> U.Vector Int)
orthoWith bnd p v1 =
  U.map (index bnd) . U.filter ((&&) <$> inRange bnd <*> p) $ U.map (add2 (unindex bnd v1)) ortho4

{-# INLINE diag4 #-}
diag4 :: U.Vector (Int, Int)
diag4 = U.fromList [(-1, 1), (1, 1), (1, -1), (-1, -1)]

-- | \(O(1)\) `G.slice` via inclusive range @[l, r]@.
{-# INLINE slice #-}
slice :: (G.Vector v a) => Int -> Int -> v a -> v a
slice !l !r !vec = G.slice l (max 0 (r - l + 1)) vec

{-# INLINE zero2 #-}
zero2 :: Int -> Int -> ((Int, Int), (Int, Int))
zero2 n1 n2 = ((0, 0), (n1 - 1, n2 - 1))

{-# INLINE zero3 #-}
zero3 :: Int -> Int -> Int -> ((Int, Int, Int), (Int, Int, Int))
zero3 n1 n2 n3 = ((0, 0, 0), (n1 - 1, n2 - 1, n3 - 1))

-- | \(O(N)\) List-like range syntax for `vector`.
--
-- >>> rangeG @U.Vector 3 5
-- [3,4,5]
{-# INLINE rangeG #-}
rangeG :: (G.Vector v Int) => Int -> Int -> v Int
rangeG !i !j = G.enumFromN i (succ j - i)

-- | \(O(N)\) Type-constrained `rangeG`.
{-# INLINE rangeV #-}
rangeV :: Int -> Int -> V.Vector Int
rangeV = rangeG

-- | \(O(N)\) Type-constrained `rangeG`.
{-# INLINE rangeU #-}
rangeU :: Int -> Int -> U.Vector Int
rangeU = rangeG

-- | \(O(N)\) Easier reverse range syntax for `vector`.
--
-- >>> rangeGR @U.Vector 3 5
-- [5,4,3]
{-# INLINE rangeGR #-}
rangeGR :: (G.Vector v Int) => Int -> Int -> v Int
rangeGR !i !j = G.enumFromStepN j (-1) (succ j - i)

-- | \(O(N)\) Type-constrained `rangeGR`.
{-# INLINE rangeVR #-}
rangeVR :: Int -> Int -> V.Vector Int
rangeVR = rangeGR

-- | \(O(N)\) Type-constrained `rangeGR`.
{-# INLINE rangeUR #-}
rangeUR :: Int -> Int -> U.Vector Int
rangeUR = rangeGR

-- | \(O(N)\) Applies the given function `n` times.
-- >>> -- 2 ^ 3
-- >>> times 3 (* 2) (1 :: Int)
-- 8
{-# INLINE times #-}
times :: Int -> (a -> a) -> a -> a
times !n !f = inner 0
  where
    inner i !s
      | i >= n = s
      | otherwise = inner (i + 1) $! f s

-- | \(O(N+M)\)
interleave :: [a] -> [a] -> [a]
interleave xs [] = xs
interleave [] ys = ys
interleave (x : xs) (y : ys) = x : y : interleave xs ys

-- -- | Returns combinations of the list taking n values.
-- -- For example, binary combinations are got by `combination 2 [0..8]`.
-- -- REMARK: This is slow. Prefer list comprehension like `x <- [1 .. n], y <- [x + 1 .. n]m ..]`.
-- combinations :: Int -> [a] -> [[a]]
-- combinations !len !elements = comb len (length elements) elements
--   where
--     comb 0 _ _ = [[]]
--     comb !r !n a@(x : xs)
--       | n == r = [a]
--       | otherwise = map (x :) (comb (r - 1) (n - 1) xs) ++ comb r (n - 1) xs
--     comb _ _ _ = error "unreachable"

-- | \(O(_{n}C_{r})\) Combinations.
-- - <https://stackoverflow.com/a/58511843>
-- - <https://zenn.dev/osushi0x/scraps/51ff0594a1e863#comment-e6b0af9b61c54c>
combs :: Int -> [a] -> [[a]]
combs _ [] = []
combs k as@(!_ : xs)
  | k == 0 = [[]]
  | k == 1 = map pure as
  | k == l = pure as
  | k > l = []
  | otherwise = run (l - 1) (k - 1) as $ combs (k - 1) xs
  where
    l = length as

    run :: Int -> Int -> [a] -> [[a]] -> [[a]]
    run n k ys cs
      | n == k = map (ys ++) cs
      | otherwise = map (q :) cs ++ run (n - 1) k qs (drop dc cs)
      where
        (!q : qs) = take (n - k + 1) ys
        dc = product [(n - k + 1) .. (n - 1)] `div` product [1 .. (k - 1)]

-- | \(O(E)\)
{-# INLINE swapDupe #-}
swapDupe :: (a, a) -> [(a, a)]
swapDupe (!x1, !x2) = [(x1, x2), (x2, x1)]

-- | \(O(1)\)
{-# INLINE add2 #-}
add2 :: (Int, Int) -> (Int, Int) -> (Int, Int)
add2 (!y, !x) = bimap (y +) (x +)

-- | \(O(1)\)
{-# INLINE sub2 #-}
sub2 :: (Int, Int) -> (Int, Int) -> (Int, Int)
sub2 (!y, !x) = bimap (y -) (x -)

-- | \(O(1)\)
{-# INLINE mul2 #-}
mul2 :: Int -> (Int, Int) -> (Int, Int)
mul2 !m = both (m *)

-- | \(O(1)\)
{-# INLINE add3 #-}
add3 :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
add3 (!z1, !y1, !x1) (!z2, !y2, !x2) = (z1 + z2, y1 + y2, x1 + x2)

-- | \(O(1)\)
{-# INLINE sub3 #-}
sub3 :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
sub3 (!z1, !y1, !x1) (!z2, !y2, !x2) = (z1 - z2, y1 - y2, x1 - x2)

-- | \(O(1)\)
{-# INLINE mul3 #-}
mul3 :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
mul3 (!z1, !y1, !x1) (!z2, !y2, !x2) = (z1 - z2, y1 - y2, x1 - x2)

-- | \(O(1)\) 180.0 degree = \p radian.
{-# INLINE toRadian #-}
toRadian :: Double -> Double
toRadian degree = degree / 180.0 * pi

-- | \(O(1)\)
{-# INLINE toDegree #-}
toDegree :: Double -> Double
toDegree rad = rad / pi * 180.0

-- | \(O(1)\)
{-# INLINE fst4 #-}
fst4 :: (a, b, c, d) -> a
fst4 (!a, !_, !_, !_) = a

-- | \(O(1)\)
{-# INLINE snd4 #-}
snd4 :: (a, b, c, d) -> b
snd4 (!_, !b, !_, !_) = b

-- | \(O(1)\)
{-# INLINE thd4 #-}
thd4 :: (a, b, c, d) -> c
thd4 (!_, !_, !c, !_) = c

-- | \(O(1)\)
{-# INLINE fth4 #-}
fth4 :: (a, b, c, d) -> d
fth4 (!_, !_, !_, !d) = d

-- | \(O(1)\)
{-# INLINE first4 #-}
first4 :: (a -> x) -> (a, b, c, d) -> (x, b, c, d)
first4 f (!a, !b, !c, !d) = (f a, b, c, d)

-- | \(O(1)\)
{-# INLINE second4 #-}
second4 :: (b -> x) -> (a, b, c, d) -> (a, x, c, d)
second4 f (!a, !b, !c, !d) = (a, f b, c, d)

-- | \(O(1)\)
{-# INLINE third4 #-}
third4 :: (c -> x) -> (a, b, c, d) -> (a, b, x, d)
third4 f (!a, !b, !c, !d) = (a, b, f c, d)

-- | \(O(1)\)
{-# INLINE fourth4 #-}
fourth4 :: (d -> x) -> (a, b, c, d) -> (a, b, c, x)
fourth4 f (!a, !b, !c, !d) = (a, b, c, f d)

-- | \(O(1)\)
fix1 :: a -> ((a -> b) -> a -> b) -> b
fix1 a f = fix f a

-- | \(O(1)\)
fix2 :: a -> b -> ((a -> b -> c) -> a -> b -> c) -> c
fix2 a b f = fix f a b

-- | \(O(1)\)
fix3 :: a -> b -> c -> ((a -> b -> c -> d) -> a -> b -> c -> d) -> d
fix3 a b c f = fix f a b c
