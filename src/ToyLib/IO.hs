{-# LANGUAGE DefaultSignatures #-}

-- | IO
--
-- >>> :{
-- convert4BS $ BS.pack "1 string 3.5 10 20 30 40" :: (Int, String, Float, VU.Vector Int)
-- :}
-- (1,"string",3.5,[10,20,30,40])

module ToyLib.IO where

import Control.Monad
import Data.Array.IArray
import Data.Bifunctor (first, second)
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS
import Data.Char (digitToInt, isSpace)
import qualified Data.Heap as H
import Data.List (unfoldr)
import Data.Maybe
import Data.Tuple.Extra hiding (first, second)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import Data.Vector.IxVector
import qualified Data.Vector.Unboxed as VU
import System.IO (stdout)

-- import ToyLib.Prelude

-- Input/parser

int :: IO Int
int = readLn

-- Lists

-- | Reads one line as a list of integers.
ints :: IO [Int]
ints = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

-- Tuples

-- | Read from a space-delimited `ByteStrtig`.
class ReadBS a where
  {-# INLINE convertBS #-}
  convertBS :: BS.ByteString -> a
  default convertBS :: (Read a) => BS.ByteString -> a
  convertBS = read . BS.unpack

  -- | For use with `VU.unfoldrExactN`.
  {-# INLINE readBS #-}
  readBS :: BS.ByteString -> (a, BS.ByteString)
  readBS !bs =
    let (!bs1, !bs2) = BS.break isSpace bs
     in (convertBS bs1, bs2)

  -- | For use with `VU.unfoldr`.
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

instance (ReadBS a, VU.Unbox a) => ReadBS (VU.Vector a) where
  {-# INLINE convertBS #-}
  convertBS = convertVG
  readBS = (,BS.empty) . convertVG
  readMayBS !bs
    | BS.null bs = Nothing
    | otherwise = Just (readBS bs)

instance (ReadBS a) => ReadBS (V.Vector a) where
  {-# INLINE convertBS #-}
  convertBS = convertVG
  readBS = (,BS.empty) . convertVG
  readMayBS !bs
    | BS.null bs = Nothing
    | otherwise = Just (readBS bs)

-- | Converrts the given `ByteString` as a vector of @a@.
convertVG :: (ReadBS a, VG.Vector v a) => BS.ByteString -> v a
convertVG = VG.unfoldr (readMayBS . BS.dropWhile isSpace)

convertV :: (ReadBS a) => BS.ByteString -> V.Vector a
convertV = convertVG

convertVU :: (ReadBS a, VU.Unbox a) => BS.ByteString -> VU.Vector a
convertVU = convertVG

-- | Converts the given `ByteString` as a vector of @a@ with @n@ elements.
convertNVG :: (ReadBS a, VG.Vector v a) => Int -> BS.ByteString -> v a
convertNVG !n = VG.unfoldrExactN n (readBS . BS.dropWhile isSpace)

convertNV :: (ReadBS a) => Int -> BS.ByteString -> V.Vector a
convertNV = convertNVG

convertNVU :: (ReadBS a, VU.Unbox a) => Int -> BS.ByteString -> VU.Vector a
convertNVU = convertNVG

convert2BS :: (ReadBS a1, ReadBS a2) => BS.ByteString -> (a1, a2)
convert2BS !bs0 =
  let (!a1, !bs1) = readBS (BS.dropWhile isSpace bs0)
      !a2 = convertBS (BS.dropWhile isSpace bs1)
   in (a1, a2)

convert3BS :: (ReadBS a1, ReadBS a2, ReadBS a3) => BS.ByteString -> (a1, a2, a3)
convert3BS !bs0 =
  let (!a1, !bs1) = readBS (BS.dropWhile isSpace bs0)
      (!a2, !bs2) = readBS (BS.dropWhile isSpace bs1)
      !a3 = convertBS (BS.dropWhile isSpace bs2)
   in (a1, a2, a3)

convert4BS :: (ReadBS a1, ReadBS a2, ReadBS a3, ReadBS a4) => BS.ByteString -> (a1, a2, a3, a4)
convert4BS !bs0 =
  let (!a1, !bs1) = readBS (BS.dropWhile isSpace bs0)
      (!a2, !bs2) = readBS (BS.dropWhile isSpace bs1)
      (!a3, !bs3) = readBS (BS.dropWhile isSpace bs2)
      !a4 = convertBS (BS.dropWhile isSpace bs3)
   in (a1, a2, a3, a4)

convert5BS :: (ReadBS a1, ReadBS a2, ReadBS a3, ReadBS a4, ReadBS a5) => BS.ByteString -> (a1, a2, a3, a4, a5)
convert5BS !bs0 =
  let (!a1, !bs1) = readBS (BS.dropWhile isSpace bs0)
      (!a2, !bs2) = readBS (BS.dropWhile isSpace bs1)
      (!a3, !bs3) = readBS (BS.dropWhile isSpace bs2)
      (!a4, !bs4) = readBS (BS.dropWhile isSpace bs3)
      !a5 = convertBS (BS.dropWhile isSpace bs4)
   in (a1, a2, a3, a4, a5)

convert6BS :: (ReadBS a1, ReadBS a2, ReadBS a3, ReadBS a4, ReadBS a5, ReadBS a6) => BS.ByteString -> (a1, a2, a3, a4, a5, a6)
convert6BS !bs0 =
  let (!a1, !bs1) = readBS (BS.dropWhile isSpace bs0)
      (!a2, !bs2) = readBS (BS.dropWhile isSpace bs1)
      (!a3, !bs3) = readBS (BS.dropWhile isSpace bs2)
      (!a4, !bs4) = readBS (BS.dropWhile isSpace bs3)
      (!a5, !bs5) = readBS (BS.dropWhile isSpace bs4)
      !a6 = convertBS (BS.dropWhile isSpace bs5)
   in (a1, a2, a3, a4, a5, a6)

-- Input/getter

get1 :: (ReadBS a1) => IO a1
get1 = convertBS <$> BS.getLine

get2 :: (ReadBS a1, ReadBS a2) => IO (a1, a2)
get2 = convert2BS <$> BS.getLine

get3 :: (ReadBS a1, ReadBS a2, ReadBS a3) => IO (a1, a2, a3)
get3 = convert3BS <$> BS.getLine

get4 :: (ReadBS a1, ReadBS a2, ReadBS a3, ReadBS a4) => IO (a1, a2, a3, a4)
get4 = convert4BS <$> BS.getLine

get5 :: (ReadBS a1, ReadBS a2, ReadBS a3, ReadBS a4, ReadBS a5) => IO (a1, a2, a3, a4, a5)
get5 = convert5BS <$> BS.getLine

get6 :: (ReadBS a1, ReadBS a2, ReadBS a3, ReadBS a4, ReadBS a5, ReadBS a6) => IO (a1, a2, a3, a4, a5, a6)
get6 = convert6BS <$> BS.getLine

ints1 :: IO Int
ints1 = get1

ints2 :: IO (Int, Int)
ints2 = get2

ints3 :: IO (Int, Int, Int)
ints3 = get3

ints4 :: IO (Int, Int, Int, Int)
ints4 = get4

ints5 :: IO (Int, Int, Int, Int, Int)
ints5 = get5

ints6 :: IO (Int, Int, Int, Int, Int, Int)
ints6 = get6

-- vectors

-- | Reads one line as an integer.
intsVG :: (VG.Vector v Int) => IO (v Int)
intsVG = VG.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

intsV :: IO (V.Vector Int)
intsV = intsVG

-- | Reads one line as a vector of integers.
intsVU :: IO (VU.Vector Int)
intsVU = intsVG

digitsVU :: IO (VU.Vector Int)
digitsVU = VU.unfoldr (fmap (first digitToInt) . BS.uncons) <$> BS.getLine

intsGrid :: Int -> Int -> IO (IxVector (Int, Int) (VU.Vector Int))
intsGrid h w = IxVector ((0, 0), (h - 1, w - 1)) . VU.concat <$> replicateM h intsVU

intsRestVG :: (VG.Vector v Int) => IO (v Int)
intsRestVG = VG.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getContents

intsRestVU :: IO (VU.Vector Int)
intsRestVU = intsRestVG

-- | Creates a graph from 1-based vertices
getGraph :: Int -> Int -> IO (Array Int [Int])
getGraph !nVerts !nEdges = accGraph . toInput <$> replicateM nEdges ints2
  where
    accGraph = accumArray @Array (flip (:)) [] (1, nVerts)
    toInput = concatMap2 $ second swap . dupe

-- Obsolute

-- | `concat` two-item tuples
concat2 :: [(a, a)] -> [a]
concat2 [] = []
concat2 ((!x, !y) : xys) = x : y : concat2 xys

concatMap2 :: (a -> (b, b)) -> [a] -> [b]
concatMap2 !f = concat2 . map f

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

-- Output

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

-- | Show as a bytestring builder.
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

showLnBSB :: (ShowBSB a) => a -> BSB.Builder
showLnBSB = (<> endlBSB) . showBSB

printBSB :: (ShowBSB a) => a -> IO ()
printBSB = putBSB . showBSB

-- | See `unwordsBSB` as example.
concatBSB :: (VG.Vector v a) => (a -> BSB.Builder) -> v a -> BSB.Builder
concatBSB f = VG.foldr' ((<>) . f) mempty

unwordsBSB :: (ShowBSB a, VG.Vector v a) => v a -> BSB.Builder
unwordsBSB = concatBSB ((<> BSB.string7 " ") . showBSB)

unlinesBSB :: (ShowBSB a, VG.Vector v a) => v a -> BSB.Builder
unlinesBSB = concatBSB showLnBSB

yn :: Bool -> String
yn b = if b then "Yes" else "No"

ynBSB :: Bool -> BSB.Builder
ynBSB b = if b then BSB.string8 "Yes" else BSB.string8 "No"

printYn :: Bool -> IO ()
printYn = putLnBSB . ynBSB
