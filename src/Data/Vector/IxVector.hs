{-# LANGUAGE RecordWildCards #-}

-- | `Ix`-based API over `vector`.
module Data.Vector.IxVector where

import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad (PrimState))
import Control.Monad.ST (ST, runST)
import Data.Bifunctor (first, second)
import Data.Core.Unindex (Unindex (..))
import Data.Foldable (for_)
import Data.Ix (Ix (inRange, index, rangeSize))
import Data.Tuple.Extra (both)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Mutable qualified as VM
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import GHC.Ix (unsafeIndex)
import GHC.Stack (HasCallStack)
import ToyLib.Debug ()
import ToyLib.Prelude (zero2)
import Prelude hiding (filter, length, map, read, replicate, zipWith)

-- | \(O(1)\) Safer `index`
{-# INLINE index' #-}
index' :: (HasCallStack, Ix i, Show i) => (i, i) -> i -> Int
index' !bnd !i
  | inRange bnd i = index bnd i
  | otherwise = error $ "index out ouf bounds: " ++ show i ++ " in " ++ show bnd

-- | N-dimensional @Vector@ or @MVector@ with `Data.Ix`.
data IxVector i v = IxVector {bounds :: !(i, i), vec :: !v}
  deriving (Show, Eq)

-- | Unboxed `IxVector` type notation.
type IxUVector i a = IxVector i (U.Vector a)

-- | Boxed `IxVector` type notation.
type IxBVector i a = IxVector i (V.Vector a)

-- | Unboxed `IxVector` type notation.
type IxMUVector s i a = IxVector i (UM.MVector s a)

-- | Boxed `IxVector` type notation.
type IxMBVector s i a = IxVector i (VM.MVector s a)

-- | Partial `IxVector` accessor
{-# INLINE (@!) #-}
(@!) :: (HasCallStack, Ix i, Show i, G.Vector v a) => IxVector i (v a) -> i -> a
(@!) IxVector {..} i = vec G.! index' bounds i

-- | Partial unsafe `IxVector` accessor
{-# INLINE (@!!) #-}
(@!!) :: (Ix i, G.Vector v a) => IxVector i (v a) -> i -> a
(@!!) IxVector {..} i = G.unsafeIndex vec (unsafeIndex bounds i)

-- | \(O(1)\) Total `IxVector` accessor
{-# INLINE (@!?) #-}
(@!?) :: (HasCallStack, Ix i, Show i, G.Vector v a) => IxVector i (v a) -> i -> Maybe a
(@!?) IxVector {..} i
  -- NOTE: `index` panics on out of range
  | inRange bounds i = Just (vec G.! index' bounds i)
  | otherwise = Nothing

-- | Total unsafe `IxVector` accessor
{-# INLINE (@!!?) #-}
(@!!?) :: (Ix i, G.Vector v a) => IxVector i (v a) -> i -> Maybe a
(@!!?) IxVector {..} i
  -- NOTE: `index` panics on out of range
  | inRange bounds i = Just (G.unsafeIndex vec (unsafeIndex bounds i))
  | otherwise = Nothing

-- | \(O(1)\)
{-# INLINE length #-}
length :: (G.Vector v a) => IxVector i (v a) -> Int
length = G.length . vec

-- | \(O(f N)\)
{-# INLINE findIndex #-}
findIndex :: (G.Vector v a, Unindex i) => (a -> Bool) -> IxVector i (v a) -> Maybe i
findIndex f IxVector {..} = unindex bounds <$> G.findIndex f vec

-- | \(O(f N)\)
{-# INLINE findIndices #-}
findIndices :: (Unindex i, G.Vector v a, G.Vector v i, G.Vector v Int) => (a -> Bool) -> IxVector i (v a) -> v i
findIndices f IxVector {..} = G.map (unindex bounds) $ G.findIndices f vec

-- | \(O(f N)\)
{-# INLINE map #-}
map :: (U.Unbox a, U.Unbox b) => (a -> b) -> IxVector i (U.Vector a) -> IxVector i (U.Vector b)
map !f IxVector {..} = IxVector bounds $ U.map f vec

-- | \(O(f N)\)
{-# INLINE imap #-}
imap :: (Unindex i, U.Unbox a, U.Unbox b) => (i -> a -> b) -> IxVector i (U.Vector a) -> IxVector i (U.Vector b)
imap !f IxVector {..} = IxVector bounds $ U.imap (f . unindex bounds) vec

-- | \(O(f N)\)
{-# INLINE filter #-}
filter :: (U.Unbox a) => (a -> Bool) -> IxVector i (U.Vector a) -> U.Vector a
filter !f IxVector {..} = U.filter f vec

-- | \(O(f N)\)
{-# INLINE ifilter #-}
ifilter :: (Unindex i, U.Unbox a) => (i -> a -> Bool) -> IxVector i (U.Vector a) -> U.Vector a
ifilter !f IxVector {..} = U.ifilter (f . unindex bounds) vec

-- | \(O(N)\)
{-# INLINE indexed #-}
indexed :: (Unindex i, U.Unbox a) => IxVector i (U.Vector a) -> U.Vector (i, a)
indexed IxVector {..} = U.imap ((,) . unindex bounds) vec

-- | \(O(N)\). Use `new` for mutable vector.
{-# INLINE replicate #-}
replicate :: (Unindex i, U.Unbox a) => (i, i) -> a -> IxUVector i a
replicate bnd x = IxVector bnd $ U.replicate (rangeSize bnd) x

-- | \(O(f N)\)
{-# INLINE zipWith #-}
zipWith :: (U.Unbox a, U.Unbox b, U.Unbox c) => (a -> b -> c) -> IxVector i (U.Vector a) -> IxVector i (U.Vector b) -> IxVector i (U.Vector c)
zipWith !f !vec1 !vec2 = IxVector bnd $ U.zipWith f (vec vec1) (vec vec2)
  where
    !bnd = bounds vec1

{-# INLINE update #-}
update :: (Ix i, Show i, U.Unbox i, U.Unbox a) => IxVector i (U.Vector a) -> U.Vector (i, a) -> IxVector i (U.Vector a)
update !vec0 !commands =
  let input1d = U.map (first (index' bnd)) commands
      -- TODO: boundary check?
      !vec1d = U.update (vec vec0) input1d
   in IxVector bnd vec1d
  where
    !bnd = bounds vec0

-- | \(O(f X)\) Altertnative to `accumulate` for `IxVector` that share the same bounds.
{-# INLINE accumulate #-}
accumulate :: (Ix i, Show i, U.Unbox i, U.Unbox a, U.Unbox b) => (a -> b -> a) -> IxVector i (U.Vector a) -> U.Vector (i, b) -> IxVector i (U.Vector a)
accumulate !f !vec0 !commands =
  let input1d = U.map (first (index' bnd)) commands
      -- TODO: boundary check?
      !vec1d = U.accumulate f (vec vec0) input1d
   in IxVector bnd vec1d
  where
    !bnd = bounds vec0

{-# INLINE create #-}
create :: (G.Vector v a) => (forall s. ST s (IxVector i (G.Mutable v s a))) -> IxVector i (v a)
create st = runST $ do
  iv <- st
  let bnd = bounds iv
  IxVector bnd <$> G.unsafeFreeze (vec iv)

-- | \(O(f N)\) `U.constructN` for `IxVector`
{-# INLINE generate #-}
generate :: (Unindex i, U.Unbox a) => (i, i) -> (i -> a) -> IxUVector i a
generate bnd f = IxVector bnd $ U.generate (rangeSize bnd) (f . unindex bnd)

-- | \(O(f N)\) `U.constructN` for `IxVector`
{-# INLINE construct #-}
construct :: (Unindex i, U.Unbox a) => (i, i) -> (IxUVector i a -> i -> a) -> IxUVector i a
construct bnd f = IxVector bnd $ U.constructN (rangeSize bnd) $ \sofar ->
  f (IxVector bnd sofar) $! unindex bnd (G.length sofar)

-- | \(O(f N)\)
{-# INLINE constructM #-}
constructM :: forall i a m. (HasCallStack, Unindex i, PrimMonad m, U.Unbox a) => (i, i) -> (IxUVector i a -> i -> m a) -> m (U.Vector a)
constructM bnd@(!_, !_) f = do
  v <- GM.new n
  v' <- G.unsafeFreeze v
  fill v' 0
  where
    !n = rangeSize bnd
    fill :: U.Vector a -> Int -> m (U.Vector a)
    fill !v i
      | i < n = do
          x <- f (IxVector bnd (G.unsafeTake i v)) (unindex bnd i)
          G.elemseq v x $ do
            v' <- G.unsafeThaw v
            GM.unsafeWrite v' i x
            v'' <- G.unsafeFreeze v'
            fill v'' (i + 1)
    fill v _ = pure v

-- | \(O(N)\)
{-# INLINE thaw #-}
thaw :: (PrimMonad m, G.Vector v a) => IxVector i (v a) -> m (IxVector i (G.Mutable v (PrimState m) a))
thaw iv = IxVector (bounds iv) <$> G.thaw (vec iv)

-- | \(O(1)\)
{-# INLINE unsafeThaw #-}
unsafeThaw :: (PrimMonad m, G.Vector v a) => IxVector i (v a) -> m (IxVector i (G.Mutable v (PrimState m) a))
unsafeThaw iv = IxVector (bounds iv) <$> G.thaw (vec iv)

-- | \(O(N)\)
{-# INLINE freeze #-}
freeze :: (PrimMonad m, G.Vector v a) => IxVector i (G.Mutable v (PrimState m) a) -> m (IxVector i (v a))
freeze iv = IxVector (bounds iv) <$> G.freeze (vec iv)

-- | \(O(1)\)
{-# INLINE unsafeFreeze #-}
unsafeFreeze :: (PrimMonad m, G.Vector v a) => IxVector i (G.Mutable v (PrimState m) a) -> m (IxVector i (v a))
unsafeFreeze iv = IxVector (bounds iv) <$> G.unsafeFreeze (vec iv)

-- | \(O(N)\) Creates a new `IxVector` with initial value.
{-# INLINE new #-}
new :: (Ix i, PrimMonad m, U.Unbox a) => (i, i) -> a -> m (IxMUVector (PrimState m) i a)
new bnd e0 = IxVector bnd <$> UM.replicate (rangeSize bnd) e0

-- | \(O(1)\) Reads a value from `IxVector`.
{-# INLINE read #-}
read :: (HasCallStack, Ix i, Show i, PrimMonad m, GM.MVector v a) => IxVector i (v (PrimState m) a) -> i -> m a
read IxVector {..} i = GM.read vec (index' bounds i)

-- | \(O(1)\) Reads a value from `IxVector`.
{-# INLINE readMaybe #-}
readMaybe :: (HasCallStack, Ix i, Show i, PrimMonad m, GM.MVector v a) => IxVector i (v (PrimState m) a) -> i -> m (Maybe a)
readMaybe IxVector {..} i
  | not (inRange bounds i) = pure Nothing
  | otherwise = Just <$> GM.read vec (index' bounds i)

-- | \(O(1)\)
{-# INLINE unsafeRead #-}
unsafeRead :: (Ix i, PrimMonad m, GM.MVector v a) => IxVector i (v (PrimState m) a) -> i -> m a
unsafeRead IxVector {..} i = GM.unsafeRead vec (unsafeIndex bounds i)

-- | \(O(1)\)
-- | Writes a value to `IxVector`.
{-# INLINE write #-}
write :: (HasCallStack, Ix i, Show i, PrimMonad m, GM.MVector v a) => IxVector i (v (PrimState m) a) -> i -> a -> m ()
write IxVector {..} i = GM.write vec (index' bounds i)

-- | \(O(1)\)
{-# INLINE unsafeWrite #-}
unsafeWrite :: (Ix i, PrimMonad m, GM.MVector v a) => IxVector i (v (PrimState m) a) -> i -> a -> m ()
unsafeWrite IxVector {..} i = GM.unsafeWrite vec (unsafeIndex bounds i)

-- | \(O(1)\)
{-# INLINE modify #-}
modify :: (HasCallStack, Ix i, PrimMonad m, GM.MVector v a) => IxVector i (v (PrimState m) a) -> (a -> a) -> i -> m ()
modify IxVector {..} !alter i = GM.modify vec alter (index bounds i)

-- FIXME: index' or something

-- | \(O(1)\)
{-# INLINE unsafeModify #-}
unsafeModify :: (Ix i, PrimMonad m, GM.MVector v a) => IxVector i (v (PrimState m) a) -> (a -> a) -> i -> m ()
unsafeModify IxVector {..} !alter i = GM.unsafeModify vec alter (unsafeIndex bounds i)

-- | \(O(1)\)
{-# INLINE modifyM #-}
modifyM :: (HasCallStack, Ix i, PrimMonad m, GM.MVector v a) => IxVector i (v (PrimState m) a) -> (a -> m a) -> i -> m ()
modifyM IxVector {..} !alter i = GM.modifyM vec alter (index bounds i)

-- | \(O(1)\)
{-# INLINE unsafeModifyM #-}
unsafeModifyM :: (Ix i, PrimMonad m, GM.MVector v a) => IxVector i (v (PrimState m) a) -> (a -> m a) -> i -> m ()
unsafeModifyM IxVector {..} !alter i = GM.unsafeModifyM vec alter (unsafeIndex bounds i)

-- | \(O(1)\)
{-# INLINE swap #-}
swap :: (HasCallStack, Ix i, PrimMonad m, GM.MVector v a) => IxVector i (v (PrimState m) a) -> i -> i -> m ()
swap IxVector {..} !i1 !i2 = GM.swap vec (index bounds i1) (index bounds i2)

-- | \(O(1)\)
{-# INLINE unsafeSwap #-}
unsafeSwap :: (Ix i, PrimMonad m, GM.MVector v a) => IxVector i (v (PrimState m) a) -> i -> i -> m ()
unsafeSwap IxVector {..} !i1 !i2 = GM.unsafeSwap vec (unsafeIndex bounds i1) (unsafeIndex bounds i2)

-- | \(O(1)\)
{-# INLINE exchange #-}
exchange :: (HasCallStack, Ix i, PrimMonad m, GM.MVector v a) => IxVector i (v (PrimState m) a) -> i -> a -> m a
exchange IxVector {..} i = GM.exchange vec (index bounds i)

-- | \(O(1)\)
{-# INLINE unsafeExchange #-}
unsafeExchange :: (Ix i, PrimMonad m, GM.MVector v a) => IxVector i (v (PrimState m) a) -> i -> a -> m a
unsafeExchange IxVector {..} i = GM.unsafeExchange vec (index bounds i)

-- | \(O(N)\) Cloning
clone :: (PrimMonad m, GM.MVector v a) => IxVector i (v (PrimState m) a) -> m (IxVector i (v (PrimState m) a))
clone IxVector {..} = do
  vec' <- GM.clone vec
  pure $ IxVector bounds vec'

-- | Extracts imos 2D seeds from rectangle add queries
{-# INLINE extractImos2d #-}
extractImos2d :: forall a. (HasCallStack, Num a, U.Unbox a) => Int -> Int -> U.Vector (Int, Int, Int, Int, a) -> IxUVector (Int, Int) a
extractImos2d h w rangeAdds = IxVector (zero2 h w) $ U.create $ do
  rect <- IxVector (zero2 h w) <$> UM.replicate (h * w) (0 :: a)
  U.forM_ rangeAdds $ \(!y1, !y2, !x1, !x2, !d) -> do
    -- TODO: run boundary check for the rect
    modify rect (+ d) (y1, x1)
    let !by = y2 < h - 1
    when by $ do
      modify rect (subtract d) (y2 + 1, x1)
    let !bx = x2 < w - 1
    when bx $ do
      modify rect (subtract d) (y1, x2 + 1)
    when (by && bx) $ do
      modify rect (+ d) (y2 + 1, x2 + 1)
  pure $ vec rect

-- | Creates a rectangle from rectangle queries
{-# INLINE imos2d #-}
imos2d :: forall a. (HasCallStack, Num a, U.Unbox a) => Int -> Int -> U.Vector (Int, Int, Int, Int, a) -> IxUVector (Int, Int) a
imos2d h w rangeAdds = IxVector (zero2 h w) $ U.create $ do
  rect <- thaw seeds

  -- row scan
  for_ [0 .. h - 1] $ \y -> do
    for_ [1 .. w - 1] $ \x -> do
      dx <- read rect (y, x - 1)
      modify rect (+ dx) (y, x)

  -- column scan
  for_ [0 .. w - 1] $ \x -> do
    for_ [1 .. h - 1] $ \y -> do
      dy <- read rect (y - 1, x)
      modify rect (+ dy) (y, x)

  pure $ vec rect
  where
    seeds = extractImos2d h w rangeAdds

-- REMARK: imos functions take (y1, y2, x1, x2, d), while `@+!` take ((y1, x1), (y2, x2))

-- | \(O(HW)\) Calculates two-dimensional cumulative sum.
--
-- WARNING: Can you really allocate/run \(O(HW)\) algorithm?
--
-- NOTE: Returns a 2D graph with one-based index with a zero row and a column inserted.
--
-- = Typical problems
-- - [ABC 331 D - Tile Pattern](https://atcoder.jp/contests/abc331/tasks/abc331_d)
{-# INLINE csum2D #-}
csum2D :: (HasCallStack, Num a, U.Unbox a) => IxUVector (Int, Int) a -> IxUVector (Int, Int) a
csum2D !gr = IxVector bnd $ U.constructN (rangeSize bnd) $ \sofar -> case unindex bnd (G.length sofar) of
  (0, _) -> 0
  (_, 0) -> 0
  (!y, !x) -> v0 + fromY + fromX - fromD
    where
      -- NOTE: Use zero-based indices on original graph access
      v0 = gr @! (y - 1, x - 1)
      fromY = IxVector bnd sofar @! (y - 1, x)
      fromX = IxVector bnd sofar @! (y, x - 1)
      fromD = IxVector bnd sofar @! (y - 1, x - 1)
  where
    -- Insert the zero row and the zero column:
    !bnd = second (both (+ 1)) (bounds gr)

-- | \(O(1)\) Returns cumulative sum in the given 2D range.
-- @
-- - - * * *
-- - - * * *
-- = = # # #
-- = = # # #
-- = = # # #
-- @
{-# INLINE (@+!) #-}
(@+!) :: (HasCallStack, Num a, U.Unbox a) => IxUVector (Int, Int) a -> ((Int, Int), (Int, Int)) -> a
(@+!) !csum ((!y1, !x1), (!y2, !x2)) = s1 + s4 - s2 - s3
  where
    -- NOTE: Using one-based indices sinces zeros are inserted
    --
    -- From top left to @#@
    !s1 = csum @! (y2 + 1, x2 + 1)
    -- From top left to @*@
    !s2 = csum @! (y1, x2 + 1)
    -- From top left to @=@
    !s3 = csum @! (y2 + 1, x1)
    -- From top left to @-@
    !s4 = csum @! (y1, x1)
