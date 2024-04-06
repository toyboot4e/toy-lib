{-# LANGUAGE DefaultSignatures #-}

-- | Super dirty slow IO
--
-- = Main procedures
--
-- `ints2`, `intsW`, `auto`, `digitsU`, `getMat` and `charsH`.
--
-- = `ReadBS` and `auto`
--
-- Primitives are `ReadBS`. Tuples of `ReadBS` are also `ReadBS`:
--
-- >>> convertBS @(Int, Char, String, Float) $ BS.pack "42 c string 2.5"
-- (42,'c',"string",2.5)
--
-- Vectors are `ReadBS` and they can also be embedded in the end of a tuple:
--
-- >>> convertBS @(Int, String, Float, U.Vector Int) $ BS.pack "1 string 3.5 10 20 30 40"
-- (1,"string",3.5,[10,20,30,40])
module ToyLib.IO where

import Control.Monad (forM_)
import Control.Monad.IO.Class
import Data.Bifunctor (first)
import Data.Bool (bool)
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS
import Data.Char (digitToInt, isSpace)
import Data.List (unfoldr)
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import Data.Vector.IxVector
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import System.IO (stdout)

-- Input/parser

int :: IO Int
int = readLn

-- Lists

-- | Reads one line as a list of integers.
ints :: IO [Int]
ints = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

-- Tuples

-- | Read from a space-delimited `ByteString`.
class ReadBS a where
  {-# INLINE convertBS #-}
  convertBS :: BS.ByteString -> a
  default convertBS :: (Read a) => BS.ByteString -> a
  convertBS = read . BS.unpack

  -- | For use with `U.unfoldrExactN`.
  {-# INLINE readBS #-}
  readBS :: BS.ByteString -> (a, BS.ByteString)
  readBS !bs =
    let (!bs1, !bs2) = BS.break isSpace bs
     in (convertBS bs1, bs2)

  -- | For use with `U.unfoldr`.
  {-# INLINE readMayBS #-}
  readMayBS :: BS.ByteString -> Maybe (a, BS.ByteString)
  readMayBS !bs
    | BS.null bs = Nothing
    | otherwise =
        let (!bs1, !bs2) = BS.break isSpace bs
         in Just (convertBS bs1, bs2)

instance ReadBS Int where
  {-# INLINE convertBS #-}
  convertBS = fst . readBS
  {-# INLINE readBS #-}
  readBS = fromJust . readMayBS
  {-# INLINE readMayBS #-}
  readMayBS = BS.readInt

instance ReadBS Integer where
  {-# INLINE convertBS #-}
  convertBS = fst . readBS
  {-# INLINE readBS #-}
  readBS = fromJust . readMayBS
  {-# INLINE readMayBS #-}
  readMayBS = BS.readInteger

instance ReadBS Float

instance ReadBS Double

instance ReadBS Char where
  {-# INLINE convertBS #-}
  convertBS = BS.head

instance ReadBS String where
  {-# INLINE convertBS #-}
  convertBS = BS.unpack

instance ReadBS BS.ByteString where
  {-# INLINE convertBS #-}
  convertBS = id

instance (ReadBS a, U.Unbox a) => ReadBS (U.Vector a) where
  {-# INLINE convertBS #-}
  convertBS = convertG
  readBS = (,BS.empty) . convertG
  readMayBS !bs
    | BS.null bs = Nothing
    | otherwise = Just (readBS bs)

instance (ReadBS a) => ReadBS (V.Vector a) where
  {-# INLINE convertBS #-}
  convertBS = convertG
  readBS = (,BS.empty) . convertG
  readMayBS !bs
    | BS.null bs = Nothing
    | otherwise = Just (readBS bs)

-- TODO: use `State` for refactoring

instance (ReadBS a1, ReadBS a2) => ReadBS (a1, a2) where
  {-# INLINE convertBS #-}
  convertBS !bs0 =
    let (!a1, !bs1) = readBS (BS.dropWhile isSpace bs0)
        !a2 = convertBS (BS.dropWhile isSpace bs1)
     in (a1, a2)
  {-# INLINE readBS #-}
  readBS = fromJust . readMayBS
  {-# INLINE readMayBS #-}
  readMayBS !bs0 = do
    (!x1, !bs1) <- readMayBS bs0
    (!x2, !bs2) <- readMayBS bs1
    Just ((x1, x2), bs2)

instance (ReadBS a1, ReadBS a2, ReadBS a3) => ReadBS (a1, a2, a3) where
  {-# INLINE convertBS #-}
  convertBS !bs0 =
    let (!a1, !bs1) = readBS (BS.dropWhile isSpace bs0)
        (!a2, !bs2) = readBS (BS.dropWhile isSpace bs1)
        !a3 = convertBS (BS.dropWhile isSpace bs2)
     in (a1, a2, a3)
  {-# INLINE readBS #-}
  readBS = fromJust . readMayBS
  {-# INLINE readMayBS #-}
  readMayBS !bs0 = do
    (!x1, !bs1) <- readMayBS bs0
    (!x2, !bs2) <- readMayBS bs1
    (!x3, !bs3) <- readMayBS bs2
    Just ((x1, x2, x3), bs3)

instance (ReadBS a1, ReadBS a2, ReadBS a3, ReadBS a4) => ReadBS (a1, a2, a3, a4) where
  {-# INLINE convertBS #-}
  convertBS !bs0 =
    let (!a1, !bs1) = readBS (BS.dropWhile isSpace bs0)
        (!a2, !bs2) = readBS (BS.dropWhile isSpace bs1)
        (!a3, !bs3) = readBS (BS.dropWhile isSpace bs2)
        !a4 = convertBS (BS.dropWhile isSpace bs3)
     in (a1, a2, a3, a4)
  {-# INLINE readBS #-}
  readBS = fromJust . readMayBS
  {-# INLINE readMayBS #-}
  readMayBS !bs0 = do
    (!x1, !bs1) <- readMayBS bs0
    (!x2, !bs2) <- readMayBS bs1
    (!x3, !bs3) <- readMayBS bs2
    (!x4, !bs4) <- readMayBS bs3
    Just ((x1, x2, x3, x4), bs4)

instance (ReadBS a1, ReadBS a2, ReadBS a3, ReadBS a4, ReadBS a5) => ReadBS (a1, a2, a3, a4, a5) where
  {-# INLINE convertBS #-}
  convertBS !bs0 =
    let (!a1, !bs1) = readBS (BS.dropWhile isSpace bs0)
        (!a2, !bs2) = readBS (BS.dropWhile isSpace bs1)
        (!a3, !bs3) = readBS (BS.dropWhile isSpace bs2)
        (!a4, !bs4) = readBS (BS.dropWhile isSpace bs3)
        !a5 = convertBS (BS.dropWhile isSpace bs4)
     in (a1, a2, a3, a4, a5)
  {-# INLINE readBS #-}
  readBS = fromJust . readMayBS
  {-# INLINE readMayBS #-}
  readMayBS !bs0 = do
    (!x1, !bs1) <- readMayBS bs0
    (!x2, !bs2) <- readMayBS bs1
    (!x3, !bs3) <- readMayBS bs2
    (!x4, !bs4) <- readMayBS bs3
    (!x5, !bs5) <- readMayBS bs4
    Just ((x1, x2, x3, x4, x5), bs5)

instance (ReadBS a1, ReadBS a2, ReadBS a3, ReadBS a4, ReadBS a5, ReadBS a6) => ReadBS (a1, a2, a3, a4, a5, a6) where
  {-# INLINE convertBS #-}
  convertBS !bs0 =
    let (!a1, !bs1) = readBS (BS.dropWhile isSpace bs0)
        (!a2, !bs2) = readBS (BS.dropWhile isSpace bs1)
        (!a3, !bs3) = readBS (BS.dropWhile isSpace bs2)
        (!a4, !bs4) = readBS (BS.dropWhile isSpace bs3)
        (!a5, !bs5) = readBS (BS.dropWhile isSpace bs4)
        !a6 = convertBS (BS.dropWhile isSpace bs5)
     in (a1, a2, a3, a4, a5, a6)
  {-# INLINE readBS #-}
  readBS = fromJust . readMayBS
  {-# INLINE readMayBS #-}
  readMayBS !bs0 = do
    (!x1, !bs1) <- readMayBS bs0
    (!x2, !bs2) <- readMayBS bs1
    (!x3, !bs3) <- readMayBS bs2
    (!x4, !bs4) <- readMayBS bs3
    (!x5, !bs5) <- readMayBS bs4
    (!x6, !bs6) <- readMayBS bs5
    Just ((x1, x2, x3, x4, x5, x6), bs6)

-- | Converrts the given `ByteString` as a vector of @a@.
convertG :: (ReadBS a, G.Vector v a) => BS.ByteString -> v a
convertG = G.unfoldr (readMayBS . BS.dropWhile isSpace)

-- | Converts the given `ByteString` as a vector of @a@ with @n@ elements.
convertNG :: (ReadBS a, G.Vector v a) => Int -> BS.ByteString -> v a
convertNG !n = G.unfoldrExactN n (readBS . BS.dropWhile isSpace)

-- Input/getter

-- | Parses one line via the `ReadBS` class.
auto :: (ReadBS a) => IO a
auto = convertBS <$> BS.getLine

ints1 :: IO Int
ints1 = auto

ints2 :: IO (Int, Int)
ints2 = auto

ints3 :: IO (Int, Int, Int)
ints3 = auto

ints4 :: IO (Int, Int, Int, Int)
ints4 = auto

ints5 :: IO (Int, Int, Int, Int, Int)
ints5 = auto

ints6 :: IO (Int, Int, Int, Int, Int, Int)
ints6 = auto

ints11 :: IO (Int, Int)
ints11 = (\(!v1, !v2) -> (v1 - 1, v2 - 1)) <$> ints2

ints110 :: IO (Int, Int, Int)
ints110 = (\(!v1, !v2, !w) -> (v1 - 1, v2 - 1, w)) <$> ints3

-- vectors

intsN :: (G.Vector v Int) => Int -> IO (v Int)
intsN !w = G.unfoldrExactN w (fromJust . BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

-- | Reads one line as an integer.
intsG :: (G.Vector v Int) => IO (v Int)
intsG = G.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

intsV :: IO (V.Vector Int)
intsV = intsG

-- | Reads one line as a vector of integers.
intsU :: IO (U.Vector Int)
intsU = intsG

digitsU :: IO (U.Vector Int)
digitsU = U.unfoldr (fmap (first digitToInt) . BS.uncons) <$> BS.getLine

-- Multi lines

-- | Converts @n@ lines of **whitespace-delimited `ByteString`** into a flat vector of type @a@.
--
-- >>> convertNBS @Int (3 * 3) $ V.map BS.pack $ V.fromList ["1 2 3", "4 5 6", "7 8 9"]
-- [1,2,3,4,5,6,7,8,9]
convertNBS :: forall a. (U.Unbox a, ReadBS a) => Int -> V.Vector BS.ByteString -> U.Vector a
convertNBS !n !bss = U.unfoldrExactN n step $ fromJust (V.uncons bss)
  where
    step :: (BS.ByteString, V.Vector BS.ByteString) -> (a, (BS.ByteString, V.Vector BS.ByteString))
    step (!cur, !rest)
      | BS.null cur' = step $ fromJust (V.uncons rest)
      | otherwise =
          let (!x, !cur'') = readBS cur'
           in (x, (cur'', rest))
      where
        -- ignore white spaces
        !cur' = BS.dropWhile isSpace cur

-- | Reads @h@ lines of stdin and converts them as HxW **whitespace-delimited `ByteString`** and
-- converts them into a flat vector of type @a@.
getHW :: (U.Unbox a, ReadBS a) => Int -> Int -> IO (U.Vector a)
getHW !h !w = convertNBS (h * w) <$> V.replicateM h BS.getLine

-- | Reads @h@ lines of stdin and converts them into a IxVector reading as HxW
-- **whitespace-separated** input.
getMat :: Int -> Int -> IO (IxVector (Int, Int) (U.Vector Int))
getMat !h !w = IxVector ((0, 0), (h - 1, w - 1)) <$> getHW h w

-- | Converts @n` lines of `ByteString` into a flat vector.
--
-- >>> U.map (== '#') . convertCharsHW $ V.map BS.pack $ V.fromList ["#.#", ".#."]
-- [True,False,True,False,True,False]
convertCharsHW :: V.Vector BS.ByteString -> U.Vector Char
convertCharsHW !bss = U.create $ do
  !vec <- UM.unsafeNew (h * w)
  V.iforM_ bss $ \y bs ->
    forM_ [0 .. w - 1] $ \x -> do
      let !char = BS.index bs x
      UM.unsafeWrite vec (w * y + x) char
  return vec
  where
    !w = BS.length (V.head bss)
    !h = V.length bss

-- | Reads @h@ lines of stdin and converts them as HxW **whitespace-delimited `ByteString`** and
-- converts them into a flat vector of type @a@.
getGrid :: Int -> Int -> IO (IxUVector (Int, Int) Char)
getGrid !h !w = IxVector ((0, 0), (h - 1, w - 1)) . convertCharsHW <$> V.replicateM h BS.getLine

-- Output

{-# INLINE endlBSB #-}
endlBSB :: BSB.Builder
endlBSB = BSB.char7 '\n'

putBSB :: (MonadIO m) => BSB.Builder -> m ()
putBSB = liftIO . BSB.hPutBuilder stdout

putLnBSB :: (MonadIO m) => BSB.Builder -> m ()
putLnBSB = liftIO . BSB.hPutBuilder stdout . (<> endlBSB)

{-# INLINE wsBSB #-}
wsBSB :: BSB.Builder
wsBSB = BSB.char7 ' '

-- ord8 :: Char -> Word8
-- ord8 = fromIntegral . fromEnum
--
-- chr8 :: Word8 -> Char
-- chr8 = toEnum . fromIntegral

-- | Show as a bytestring builder.
class ShowBSB a where
  showBSB :: a -> BSB.Builder
  default showBSB :: (Show a) => a -> BSB.Builder
  showBSB = BSB.string8 . show

-- TODO: deriving?

instance ShowBSB Int where
  showBSB = BSB.intDec

instance ShowBSB Integer where
  showBSB = BSB.integerDec

instance ShowBSB Float where
  showBSB = BSB.floatDec

instance ShowBSB Double where
  showBSB = BSB.doubleDec

instance ShowBSB Char where
  showBSB = BSB.char7

instance ShowBSB String where
  -- TODO: string7 vs string8
  showBSB = BSB.string8

instance ShowBSB BS.ByteString where
  showBSB = BSB.byteString

instance (ShowBSB a, ShowBSB b) => ShowBSB (a, b) where
  showBSB (!a, !b) = showBSB a <> BSB.string7 " " <> showBSB b

showLnBSB :: (ShowBSB a) => a -> BSB.Builder
showLnBSB = (<> endlBSB) . showBSB

printBSB :: (ShowBSB a, MonadIO m) => a -> m ()
printBSB = putBSB . showLnBSB

concatBSB :: (G.Vector v a, ShowBSB a) => v a -> BSB.Builder
concatBSB = G.foldMap showBSB

intersperseBSB :: (G.Vector v a, ShowBSB a) => BSB.Builder -> v a -> BSB.Builder
intersperseBSB del vec
  | G.null vec = mempty
  | otherwise = showBSB (G.head vec) <> G.foldMap ((del <>) . showBSB) (G.tail vec)

unwordsBSB :: (ShowBSB a, G.Vector v a) => v a -> BSB.Builder
unwordsBSB = intersperseBSB wsBSB

unlinesBSB :: (ShowBSB a, G.Vector v a) => v a -> BSB.Builder
unlinesBSB = intersperseBSB endlBSB

yn :: Bool -> String
yn = bool "No" "Yes"

ynBSB :: Bool -> BSB.Builder
ynBSB = bool (BSB.string8 "No") (BSB.string8 "Yes")

printYn :: (MonadIO m) => Bool -> m ()
printYn = putLnBSB . ynBSB

printList :: (ShowBSB a, U.Unbox a, MonadIO m) => [a] -> m ()
printList = putLnBSB . unwordsBSB . U.fromList

putList :: (ShowBSB a, U.Unbox a, MonadIO m) => [a] -> m ()
putList = putBSB . unwordsBSB . U.fromList

printVec :: (ShowBSB a, G.Vector v a, MonadIO m) => v a -> m ()
printVec = putLnBSB . unwordsBSB

putVec :: (ShowBSB a, G.Vector v a, MonadIO m) => v a -> m ()
putVec = putBSB . unwordsBSB

printGrid :: (MonadIO m) => IxUVector (Int, Int) Char -> m ()
printGrid = putBSB . showGridBSB

showGridBSB :: IxUVector (Int, Int) Char -> BSB.Builder
showGridBSB mat = G.foldMap ((<> endlBSB) . concatBSB) rows
  where
    ((!y1, !x1), (!y2, !x2)) = boundsIV mat
    !h = y2 + 1 - y1
    !w = x2 + 1 - x1
    rows = V.unfoldrExactN h (U.splitAt w) (vecIV mat)

printMat :: (ShowBSB a, U.Unbox a, MonadIO m) => IxUVector (Int, Int) a -> m ()
printMat = putBSB . showMatBSB

showMatBSB :: (ShowBSB a, U.Unbox a) => IxUVector (Int, Int) a -> BSB.Builder
showMatBSB mat = G.foldMap ((<> endlBSB) . unwordsBSB) rows
  where
    ((!y1, !x1), (!y2, !x2)) = boundsIV mat
    !h = y2 + 1 - y1
    !w = x2 + 1 - x1
    rows = V.unfoldrExactN h (U.splitAt w) (vecIV mat)
