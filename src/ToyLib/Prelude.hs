{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}

module ToyLib.Prelude where

import Control.Monad
import Control.Monad.Primitive
import Data.Array.IArray
import Data.Bifunctor
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS
import Data.Char (digitToInt, isSpace)
import qualified Data.Heap as H
import Data.List
import Data.Tuple.Extra hiding (first, second)
import qualified Data.Vector as V
import qualified Data.Vector.Fusion.Stream.Monadic as MS
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import Data.Vector.IxVector
import qualified Data.Vector.Unboxed as VU
import Debug.Trace
import System.IO (stdout)

-- {{{ Prelude utilities

-- | From more recent GHC
clamp :: (Ord a) => (a, a) -> a -> a
clamp (!low, !high) !a = min high (max a low)

flipOrder :: Ordering -> Ordering
flipOrder = \case
  GT -> LT
  LT -> GT
  EQ -> EQ

square :: Num a => a -> a
square !x = x * x

-- | Returns chunks of size `n`.
-- TODO: Just use `Data.List.Extra.chunksOf`.
chunks :: Int -> [a] -> [[a]]
chunks n = inner
  where
    inner [] = []
    inner xs =
      let (!g, !rest) = splitAt n xs
       in g : inner rest

-- }}}

-- {{{ More extras

-- | Two-variable function compositon.
(.:) :: (b -> c) -> (a1 -> a2 -> b) -> (a1 -> a2 -> c)
(.:) = (.) . (.)

-- | Three-variable function compositon.
(.:.) :: (b -> c) -> (a1 -> a2 -> a3 -> b) -> (a1 -> a2 -> a3 -> c)
(.:.) = (.) . (.) . (.)

-- | Strict funciton composition.
{-# INLINE (.!) #-}
(.!) :: (b -> c) -> (a -> b) -> a -> c
(.!) = (.) . ($!)
infixr 9 .!

foldFor :: (Foldable t) => b -> t a -> (b -> a -> b) -> b
foldFor !s0 !xs !f = foldl' f s0 xs

foldForVG :: (VG.Vector v a) => b -> v a -> (b -> a -> b) -> b
foldForVG !s0 !xs !f = VG.foldl' f s0 xs

foldForM :: (Foldable t, Monad m) => b -> t a -> (b -> a -> m b) -> m b
foldForM !s0 !xs !m = foldM m s0 xs

foldForMVG :: (PrimMonad m, VG.Vector v a) => b -> v a -> (b -> a -> m b) -> m b
foldForMVG !s0 !xs !m = VG.foldM' m s0 xs

foldForMMS :: Monad m => a -> MS.Stream m b -> (a -> b -> m a) -> m a
foldForMMS !s0 !xs !f = MS.foldM' f s0 xs

-- TODO: `minimumMay` from `mono-traversable`?
-- or copy the `safe` packge: <https://hackage.haskell.org/package/safe-0.3.14/docs/Safe-Foldable.html>

minimumOr :: (Ord a, VU.Unbox a) => a -> VU.Vector a -> a
minimumOr !orValue !xs =
  if VU.null xs
    then orValue
    else VU.minimum xs

maximumOr :: (Ord a, VU.Unbox a) => a -> VU.Vector a -> a
maximumOr !orValue !xs =
  if VU.null xs
    then orValue
    else VU.maximum xs

safeHead :: (VU.Unbox a) => VU.Vector a -> Maybe a
safeHead vec = if VU.null vec then Nothing else Just $! VU.head vec

safeLast :: (VU.Unbox a) => VU.Vector a -> Maybe a
safeLast vec = if VU.null vec then Nothing else Just $! VU.last vec

-- }}}

-- {{{ Libary complements

{-# INLINE rangeVG #-}
rangeVG :: (VG.Vector v Int) => Int -> Int -> v Int
rangeVG !i !j = VG.enumFromN i (succ j - i)

{-# INLINE rangeV #-}
rangeV :: Int -> Int -> V.Vector Int
rangeV = rangeVG

{-# INLINE rangeVU #-}
rangeVU :: Int -> Int -> VU.Vector Int
rangeVU = rangeVG

-- | `rangeVG` in reverse.
{-# INLINE rangeVGR #-}
rangeVGR :: (VG.Vector v Int) => Int -> Int -> v Int
rangeVGR !i !j = VG.enumFromStepN (pred j) (-1) (succ j - i)

{-# INLINE rangeVR #-}
rangeVR :: Int -> Int -> V.Vector Int
rangeVR = rangeVGR

{-# INLINE rangeVUR #-}
rangeVUR :: Int -> Int -> VU.Vector Int
rangeVUR = rangeVGR

-- | @relaxMany !f !vec0 !input !expander@ ~ @VG.accumulate f vec0 $ VG.concatMap expander input@
relaxMany :: (VG.Vector v a, VG.Vector v (Int, a), VG.Vector v b) => (a -> a -> a) -> v a -> v b -> (b -> v (Int, a)) -> v a
relaxMany !relax !vec0 !input !expander = VG.create $ do
  !vec <- VG.unsafeThaw vec0

  VG.forM_ input $ \x -> do
    VG.forM_ (expander x) $ \(!i, !x') -> do
      VGM.modify vec (`relax` x') i

  return vec

-- | @cojna (`stream`)
{-# INLINE [1] rangeMS #-}
rangeMS :: (Monad m) => Int -> Int -> MS.Stream m Int
rangeMS !l !r = MS.Stream step l
  where
    {-# INLINE [0] step #-}
    step x
      | x <= r = return $ MS.Yield x (x + 1)
      | otherwise = return MS.Done

-- | @cojna (`streamR`)
{-# INLINE [1] rangeMSR #-}
rangeMSR :: (Monad m) => Int -> Int -> MS.Stream m Int
rangeMSR !l !r = MS.Stream step r
  where
    {-# INLINE [0] step #-}
    step x
      | x >= l = return $ MS.Yield x (x - 1)
      | otherwise = return MS.Done

-- | `forM` over monadic stream in the vector package.
-- | NOTE: This is for side effects only. I don't know how to use `MS.mapM` yet.
{-# INLINE forMS_ #-}
forMS_ :: (Monad m) => MS.Stream m Int -> (Int -> m ()) -> m ()
forMS_ = flip MS.mapM_

{-# INLINE repM_ #-}
repM_ :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
repM_ !l !r !act = inner l
  where
    inner !i
      | i > r = return ()
      | otherwise = act i >> inner (succ i)

{-# INLINE repRM_ #-}
repRM_ :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
repRM_ !l !r !act = inner r
  where
    inner !i
      | i < l = return ()
      | otherwise = act i >> inner (pred i)

-- | @constructN@ with initial value for index zero.
constructN0 :: (VU.Unbox a) => a -> Int -> (VU.Vector a -> a) -> VU.Vector a
constructN0 !x0 !n !f = VU.constructN n $ \vec ->
  if VU.null vec
    then x0
    else f vec

-- }}}

-- {{{ cheatsheet

-- Option - Maybe cheatsheet
-- https://notes.iveselov.info/programming/cheatsheet-rust-option-vs-haskell-maybe

-- compress deduplicates sorted list, nub deduplicates non-sorted list
-- TODO: std?
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x : xs) = x : compress (dropWhile (== x) xs)

-- | Runs the given function `n` times.
{-# INLINE times #-}
times :: Int -> (a -> a) -> a -> a
times !n !f !s0 = snd $! until ((== n) . fst) (bimap succ f) (0 :: Int, s0)

-- -- | Returns combinations of the list taking n values.
-- -- | For example, binary combinations are got by `combination 2 [0..8]`.
-- -- | REMARK: This is slow. Prefer list comprehension like `x <- [1 .. n], y <- [x + 1 .. n]m ..]`.
-- combinations :: Int -> [a] -> [[a]]
-- combinations !len !elements = comb len (length elements) elements
--   where
--     comb 0 _ _ = [[]]
--     comb !r !n a@(x : xs)
--       | n == r = [a]
--       | otherwise = map (x :) (comb (r - 1) (n - 1) xs) ++ comb r (n - 1) xs
--     comb _ _ _ = error "unreachable"

-- | Combinations.
-- - <https://stackoverflow.com/a/58511843>
-- - <https://zenn.dev/osushi0x/scraps/51ff0594a1e863#comment-e6b0af9b61c54c>
combs :: Int -> [a] -> [[a]]
combs _ [] = error "given empty list"
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

-- | Returns inclusive ranges that satisfy the given `check`.
-- FIXME: Use a simpler, cheaper implementation
twoPointers :: Int -> ((Int, Int) -> Bool) -> [(Int, Int)]
twoPointers !n !check = inner (0, 0)
  where
    inner (!l, !_) | l >= n = []
    inner (!l, !r)
      | check (l, r) =
          let (!l', !r') = until (not . peekCheck) (second succ) (l, r)
           in (l', r') : inner (succ l', max l' r')
      | otherwise = inner (succ l, max (succ l) r)
    peekCheck (!_, !r) | r == pred n = False
    peekCheck (!l, !r) = check (l, succ r)

-- }}}

-- {{{ Tuples

tuple2 :: [a] -> (a, a)
tuple2 [!a1, !a2] = (a1, a2)
tuple2 _ = error "not a two-item list"

tuple3 :: [a] -> (a, a, a)
tuple3 [!a1, !a2, !a3] = (a1, a2, a3)
tuple3 _ = error "not a three-item list"

tuple4 :: [a] -> (a, a, a, a)
tuple4 [!a1, !a2, !a3, !a4] = (a1, a2, a3, a4)
tuple4 _ = error "not a four-item list"

tuple5 :: [a] -> (a, a, a, a, a)
tuple5 [!a1, !a2, !a3, !a4, !a5] = (a1, a2, a3, a4, a5)
tuple5 _ = error "not a five-item list"

tuple6 :: [a] -> (a, a, a, a, a, a)
tuple6 [!a1, !a2, !a3, !a4, !a5, !a6] = (a1, a2, a3, a4, a5, a6)
tuple6 _ = error "not a six-item list"

ints2 :: IO (Int, Int)
ints2 = tuple2 <$> ints

ints3 :: IO (Int, Int, Int)
ints3 = tuple3 <$> ints

ints4 :: IO (Int, Int, Int, Int)
ints4 = tuple4 <$> ints

ints5 :: IO (Int, Int, Int, Int, Int)
ints5 = tuple5 <$> ints

ints6 :: IO (Int, Int, Int, Int, Int, Int)
ints6 = tuple6 <$> ints

yn :: Bool -> String
yn b = if b then "Yes" else "No"

printYn :: Bool -> IO ()
printYn = putStrLn . yn

-- | `concat` two-item tuples
concat2 :: [(a, a)] -> [a]
concat2 [] = []
concat2 ((!x, !y) : xys) = x : y : concat2 xys

concatMap2 :: (a -> (b, b)) -> [a] -> [b]
concatMap2 !f = concat2 . map f

swapDupe :: (a, a) -> ((a, a), (a, a))
swapDupe = second swap . dupe

add2 :: (Int, Int) -> (Int, Int) -> (Int, Int)
add2 (!y, !x) = bimap (y +) (x +)

sub2 :: (Int, Int) -> (Int, Int) -> (Int, Int)
sub2 (!y, !x) = bimap (y -) (x -)

mul2 :: Int -> (Int, Int) -> (Int, Int)
mul2 !m = both (m *)

add3 :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
add3 (!z1, !y1, !x1) (!z2, !y2, !x2) = (z1 + z2, y1 + y2, x1 + x2)

sub3 :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
sub3 (!z1, !y1, !x1) (!z2, !y2, !x2) = (z1 - z2, y1 - y2, x1 - x2)

mul3 :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
mul3 (!z1, !y1, !x1) (!z2, !y2, !x2) = (z1 - z2, y1 - y2, x1 - x2)

-- 180.0 degree = \p radian
toRadian :: Double -> Double
toRadian degree = degree / 180.0 * pi

toDegree :: Double -> Double
toDegree rad = rad / pi * 180.0

fst4 :: (a, b, c, d) -> a
fst4 (!a, !_, !_, !_) = a

snd4 :: (a, b, c, d) -> b
snd4 (!_, !b, !_, !_) = b

thd4 :: (a, b, c, d) -> c
thd4 (!_, !_, !c, !_) = c

fth4 :: (a, b, c, d) -> d
fth4 (!_, !_, !_, !d) = d

-- }}}

-- {{{ Input

-- TODO: Consider separating parser from reader

-- | Reads one line as an integer.
int :: IO Int
int = readLn

-- | Reads one line as a list of integers.
ints :: IO [Int]
ints = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

intsVG :: VG.Vector v Int => IO (v Int)
intsVG = VG.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

intsV :: IO (V.Vector Int)
intsV = intsVG

-- | Reads one line as a vector of integers.
intsVU :: IO (VU.Vector Int)
intsVU = intsVG

-- | Reads one line as a vector of digits.
digitsVU :: IO (VU.Vector Int)
digitsVU = VU.unfoldr (fmap (first digitToInt) . BS.uncons) <$> BS.getLine

-- | FIXME: Faster implementation
intsN :: Int -> IO [Int]
intsN n = concat <$> replicateM n ints

-- | FIXME: Faster implementation
intsNVU :: Int -> IO (VU.Vector Int)
intsNVU n = VU.fromList . concat <$> replicateM n ints

intsGrid :: Int -> Int -> IO (IxVector (Int, Int) (VU.Vector Int))
intsGrid h w = IxVector ((0, 0), (h - 1, w - 1)) <$> intsNVU h

intsRestVG :: VG.Vector v Int => IO (v Int)
intsRestVG = VG.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getContents

intsRestVU :: IO (VU.Vector Int)
intsRestVU = intsRestVG

-- | Creates a graph from 1-based vertices
getGraph :: Int -> Int -> IO (Array Int [Int])
getGraph !nVerts !nEdges = accGraph . toInput <$> replicateM nEdges ints
  where
    accGraph = accumArray @Array (flip (:)) [] (1, nVerts)
    toInput = concatMap2 $ second swap . dupe . tuple2

-- | Creates a weightend graph from 1-based vertices
getWGraph :: Int -> Int -> IO (Array Int [H.Entry Int Int])
getWGraph !nVerts !nEdges = accGraph . toInput <$> replicateM nEdges ints
  where
    accGraph = accumArray @Array (flip (:)) [] (1, nVerts)
    toInput = concatMap2 $ \[!a, !b, !cost] -> ((a, H.Entry cost b), (b, H.Entry cost a))

-- | Creates a weightend graph from 1-based vertices
getWGraph0 :: Int -> Int -> IO (Array Int [H.Entry Int Int])
getWGraph0 !nVerts !nEdges = accGraph . toInput <$> replicateM nEdges ints
  where
    accGraph = accumArray @Array (flip (:)) [] (0, pred nVerts)
    toInput = concatMap2 $ \[!a, !b, !cost] -> ((pred a, H.Entry cost (pred b)), (pred b, H.Entry cost (pred a)))

-- }}}

-- {{{ Output

{-# INLINE endlBSB #-}
endlBSB :: BSB.Builder
endlBSB = BSB.char7 '\n'

putBSB :: BSB.Builder -> IO ()
putBSB = BSB.hPutBuilder stdout

putLnBSB :: BSB.Builder -> IO ()
putLnBSB = BSB.hPutBuilder stdout . (<> endlBSB)

-- ord8 :: Char -> Word8
-- ord8 = fromIntegral . fromEnum
--
-- chr8 :: Word8 -> Char
-- chr8 = toEnum . fromIntegral

-- | Show as a bytestring builder
class ShowBSB a where
  showBSB :: a -> BSB.Builder
  default showBSB :: (Show a) => a -> BSB.Builder
  showBSB = BSB.string8 . show

instance ShowBSB Int where
  showBSB = BSB.intDec

instance ShowBSB Integer where
  showBSB = BSB.integerDec

instance ShowBSB Float where
  showBSB = BSB.floatDec

instance ShowBSB Double where
  showBSB = BSB.doubleDec

showLnBSB :: ShowBSB a => a -> BSB.Builder
showLnBSB = (<> endlBSB) . showBSB

printBSB :: ShowBSB a => a -> IO ()
printBSB = putBSB . showBSB

-- | See `unwordsBSB` as example.
concatBSB :: (VG.Vector v a) => (a -> BSB.Builder) -> v a -> BSB.Builder
concatBSB f = VG.foldr' ((<>) . f) mempty

-- FIXME: unnecessary whitespace at the end?
unwordsBSB :: (ShowBSB a, VG.Vector v a) => v a -> BSB.Builder
unwordsBSB = concatBSB ((<> BSB.string7 " ") . showBSB)

unlinesBSB :: (ShowBSB a, VG.Vector v a) => v a -> BSB.Builder
unlinesBSB = concatBSB showLnBSB

-- }}}

-- {{{ Trace

-- TODO: merge them with `dbg` series.

traceMat2D :: (IArray a e, Ix i, Show e) => a (i, i) e -> ()
traceMat2D !mat = traceSubMat2D mat (bounds mat)

traceSubMat2D :: (IArray a e, Ix i, Show e) => a (i, i) e -> ((i, i), (i, i)) -> ()
traceSubMat2D !mat ((!y0, !x0), (!yEnd, !xEnd)) =
  let !_ = foldl' step () (range ys)
   in ()
  where
    !xs = (y0, yEnd)
    !ys = (x0, xEnd)
    step !_ !y = traceShow (map (\ !x -> mat ! (y, x)) (range xs)) ()

-- }}}

-- | Inaccurate, but fast `Int` square root.
-- TODO: Fast and accurate implementation
isqrt :: Int -> Int
isqrt = round @Double . sqrt . fromIntegral
