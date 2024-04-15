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
module ToyLib.ShowBSB where

import Control.Monad.IO.Class
import Data.Bool (bool)
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import Data.Vector.IxVector
import qualified Data.Vector.Unboxed as U
import System.IO (stdout)

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

instance ShowBSB BSB.Builder where
  showBSB = id

instance (ShowBSB a, ShowBSB b) => ShowBSB (a, b) where
  showBSB (!a, !b) = showBSB a <> BSB.string7 " " <> showBSB b

instance (ShowBSB a, ShowBSB b, ShowBSB c) => ShowBSB (a, b, c) where
  showBSB (!a, !b, c) = showBSB a <> BSB.string7 " " <> showBSB b <> BSB.string7 " " <> showBSB c

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
