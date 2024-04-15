-- | `StateT` -based parser.
module ToyLib.Parser where

import Control.Monad.State.Class
import Control.Monad.Trans.State.Strict (State, evalState, StateT, evalStateT)
import Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as BS
import Data.Char (digitToInt, isSpace)
import Data.Maybe
import Data.Vector.IxVector
import qualified Data.Vector.Unboxed as U

-- | Reads the whole stdin and runs the user program.
runIO :: StateT BS.ByteString IO a -> IO a
runIO = (BS.getContents >>=) . evalStateT

-- | Reads a file runs the user program.
runFileIO :: StateT BS.ByteString IO a -> String -> IO a
runFileIO f path = evalStateT f =<< BS.readFile path

-- * Primitives

-- | Parses an `Int`.
int' :: (MonadState BS.ByteString m) => m Int
int' = state $ fromJust . BS.readInt . BS.dropWhile isSpace

-- | Parses an `Int` and substracts one.
int1' :: (MonadState BS.ByteString m) => m Int
int1' = subtract 1 <$> int'

char' :: (MonadState BS.ByteString m) => m Char
char' = state $ fromJust . BS.uncons . BS.dropWhile isSpace

word' :: (MonadState BS.ByteString m) => m BS.ByteString
word' = state $ BS.break isSpace . BS.dropWhile isSpace

-- | Parsers an `Int` and converts it into a `Double`.
--
-- TODO: read and word'
double' :: (MonadState BS.ByteString m) => m Double
double' = fromIntegral <$> int'

-- * Tuples

ints2' :: (MonadState BS.ByteString m) => m (Int, Int)
ints2' = (,) <$> int' <*> int'

ints11' :: (MonadState BS.ByteString m) => m (Int, Int)
ints11' = (,) <$> int1' <*> int1'

ints3' :: (MonadState BS.ByteString m) => m (Int, Int, Int)
ints3' = (,,) <$> int' <*> int' <*> int'

ints110' :: (MonadState BS.ByteString m) => m (Int, Int, Int)
ints110' = (,,) <$> int1' <*> int1' <*> int'

ints4' :: (MonadState BS.ByteString m) => m (Int, Int, Int, Int)
ints4' = (,,,) <$> int' <*> int' <*> int' <*> int'

ints5' :: (MonadState BS.ByteString m) => m (Int, Int, Int, Int, Int)
ints5' = (,,,,) <$> int' <*> int' <*> int' <*> int' <*> int'

ints6' :: (MonadState BS.ByteString m) => m (Int, Int, Int, Int, Int, Int)
ints6' = (,,,,,) <$> int' <*> int' <*> int' <*> int' <*> int' <*> int'

-- * Readers

-- | Reads one line from the state.
line' :: (MonadState BS.ByteString m) => m BS.ByteString
line' = state $ BS.span (/= '\n') . BS.dropWhile isSpace

-- | Reads one line from the state and runs a pure parser for it.
withLine' :: (MonadState BS.ByteString m) => State BS.ByteString a -> m a
withLine' f = evalState f <$> line'

-- * More

-- | Reads an unboxed vector.
intsU' :: (MonadState BS.ByteString m) => m (U.Vector Int)
intsU' = U.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> line'

-- | Reads an unboxed vector with size @n@.
intsN' :: (MonadState BS.ByteString m) => Int -> m (U.Vector Int)
-- intsN' n = withLine' $ U.replicateM n int'
intsN' n = U.unfoldrExactN n (fromJust . BS.readInt . BS.dropWhile isSpace) <$> line'

digitsU' :: (MonadState BS.ByteString m) => m (U.Vector Int)
digitsU' = U.unfoldr (fmap (first digitToInt) . BS.uncons) <$> line'

-- | Reads next @h * w@ elements as matrix of type @a@.
getMat' :: (MonadState BS.ByteString m) => Int -> Int -> m (IxVector (Int, Int) (U.Vector Int))
getMat' !h !w = IxVector ((0, 0), (h - 1, w - 1)) <$> U.replicateM (h * w) int'

-- | Reads next @h * w@ elements as a char-based grid.
getGrid' :: (MonadState BS.ByteString m) => Int -> Int -> m (IxUVector (Int, Int) Char)
getGrid' !h !w = IxVector ((0, 0), (h - 1, w - 1)) <$> U.replicateM (h * w) char'
