-- | `StateT` -based parser.
--
-- - TODO: Replace my `IO` module.
-- - TODO: try strict tuple evaluation and compare performacne
module ToyLib.Parser where

import Control.Monad
import Control.Monad.State.Class
import Control.Monad.Trans.State.Strict (State, StateT, evalState, evalStateT, execState, execStateT, runState, runStateT)
import Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as BS
import Data.Char (digitToInt, isSpace)
import Data.Maybe
import qualified Data.Vector as V
import Data.Vector.IxVector
import qualified Data.Vector.Unboxed as U
import ToyLib.IO

-- | Reads the whole stdin and runs the user program.
runIO :: StateT BS.ByteString IO a -> IO ()
runIO = (BS.getContents >>=) . evalStateT . void

-- | Parses an `Int`.
int' :: (MonadState BS.ByteString m) => m Int
int' = state $ fromJust . BS.readInt . BS.dropWhile isSpace

-- | Parses an `Int` and substracts one.
int1' :: (MonadState BS.ByteString m) => m Int
int1' = subtract 1 <$> int'

-- | Parsers an `Int` and converts it into a `Double`.
--
-- TODO: read and word'
double' :: (MonadState BS.ByteString m) => m Double
double' = fromIntegral <$> int'

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

-- | Reads one line from the state.
getLine' :: (MonadState BS.ByteString m) => m BS.ByteString
getLine' = state $ \bs ->
  let (!line, !rest) = BS.span (== '\n') $ BS.dropWhile isSpace bs
   in (rest, line)

-- | Reads one line from the state and runs a pure parser for it.
withLine' :: (MonadState BS.ByteString m) => State BS.ByteString a -> m a
withLine' f = evalState f <$> getLine'

-- | Reads an unboxed vector.
intsU' :: (MonadState BS.ByteString m) => m (U.Vector Int)
intsU' = U.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> getLine'

-- | Reads an unboxed vector with size @n@.
intsN' :: (MonadState BS.ByteString m) => Int -> m (U.Vector Int)
-- intsN' n = withLine' $ U.replicateM n int'
intsN' n = U.unfoldrExactN n (fromJust . BS.readInt . BS.dropWhile isSpace) <$> getLine'

digitsU' :: (MonadState BS.ByteString m) => m (U.Vector Int)
digitsU' = U.unfoldr (fmap (first digitToInt) . BS.uncons) <$> getLine'

-- | Parses one line via the `ReadBS` class.
auto' :: (ReadBS a, MonadState BS.ByteString m) => m a
auto' = convertBS <$> getLine'

-- | Reads @h@ lines of stdin and converts them as HxW **whitespace-delimited `ByteString`** and
-- converts them into a flat vector of type @a@.
getHW' :: (U.Unbox a, ReadBS a, MonadState BS.ByteString m) => Int -> Int -> m (U.Vector a)
getHW' !h !w = convertNBS (h * w) <$> V.replicateM h getLine'

-- | Reads @h@ lines of stdin and converts them into a IxVector reading as HxW
-- **whitespace-separated** input.
getMat' :: (MonadState BS.ByteString m) => Int -> Int -> m (IxVector (Int, Int) (U.Vector Int))
getMat' !h !w = IxVector ((0, 0), (h - 1, w - 1)) <$> U.replicateM (h * w) int'

-- | Reads @h@ lines of stdin and converts them as HxW **whitespace-delimited `ByteString`** and
-- converts them into a flat vector of type @a@.
getGrid' :: (MonadState BS.ByteString m) => Int -> Int -> m (IxUVector (Int, Int) Char)
getGrid' !h !w = IxVector ((0, 0), (h - 1, w - 1)) . convertCharsHW <$> V.replicateM h getLine'
