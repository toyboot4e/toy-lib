-- | `StateT` -based parser.
module ToyLib.Parser where

import Control.Monad.State.Class
import Control.Monad.Trans.State.Strict (State, evalState, StateT, evalStateT)
import Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as BS
import Data.Char (digitToInt, isSpace)
import Data.List (unfoldr)
import Data.Maybe
import qualified Data.Vector.Unboxed as U

-- | Reads the whole stdin and runs the user program.
{-# INLINE runIO #-}
runIO :: StateT BS.ByteString IO a -> IO a
runIO = (BS.getContents >>=) . evalStateT

-- | Reads a file runs the user program.
{-# INLINE runFileIO #-}
runFileIO :: StateT BS.ByteString IO a -> String -> IO a
runFileIO f path = evalStateT f =<< BS.readFile path

-- * Primitives

-- | Parses an `Int`.
{-# INLINE int' #-}
int' :: (MonadState BS.ByteString m) => m Int
int' = state $ fromJust . BS.readInt . BS.dropSpace

-- | Parses an `Int` and substracts one.
{-# INLINE int1' #-}
int1' :: (MonadState BS.ByteString m) => m Int
int1' = subtract 1 <$> int'

{-# INLINE char' #-}
char' :: (MonadState BS.ByteString m) => m Char
char' = state $ fromJust . BS.uncons . BS.dropSpace

{-# INLINE word' #-}
word' :: (MonadState BS.ByteString m) => m BS.ByteString
word' = state $ BS.break isSpace . BS.dropSpace

-- | Parsers an `Int` and converts it into a `Double`.
double' :: (MonadState BS.ByteString m) => m Double
double' = read . BS.unpack <$> word'

-- * Tuples

{-# INLINE ints2' #-}
ints2' :: (MonadState BS.ByteString m) => m (Int, Int)
ints2' = (,) <$> int' <*> int'

{-# INLINE ints11' #-}
ints11' :: (MonadState BS.ByteString m) => m (Int, Int)
ints11' = (,) <$> int1' <*> int1'

{-# INLINE ints3' #-}
ints3' :: (MonadState BS.ByteString m) => m (Int, Int, Int)
ints3' = (,,) <$> int' <*> int' <*> int'

{-# INLINE ints110' #-}
ints110' :: (MonadState BS.ByteString m) => m (Int, Int, Int)
ints110' = (,,) <$> int1' <*> int1' <*> int'

{-# INLINE ints011' #-}
ints011' :: (MonadState BS.ByteString m) => m (Int, Int, Int)
ints011' = (,,) <$> int' <*> int1' <*> int1'

{-# INLINE ints111' #-}
ints111' :: (MonadState BS.ByteString m) => m (Int, Int, Int)
ints111' = (,,) <$> int1' <*> int1' <*> int1'

{-# INLINE ints4' #-}
ints4' :: (MonadState BS.ByteString m) => m (Int, Int, Int, Int)
ints4' = (,,,) <$> int' <*> int' <*> int' <*> int'

{-# INLINE ints5' #-}
ints5' :: (MonadState BS.ByteString m) => m (Int, Int, Int, Int, Int)
ints5' = (,,,,) <$> int' <*> int' <*> int' <*> int' <*> int'

{-# INLINE ints6' #-}
ints6' :: (MonadState BS.ByteString m) => m (Int, Int, Int, Int, Int, Int)
ints6' = (,,,,,) <$> int' <*> int' <*> int' <*> int' <*> int' <*> int'

-- * Readers

-- | Reads one line from the state. FIXME: It ignores empty lines.
{-# INLINE line' #-}
line' :: (MonadState BS.ByteString m) => m BS.ByteString
line' = state $ BS.span (/= '\n') . BS.dropSpace

-- | Reads one line from the state.
{-# INLINE lineU' #-}
lineU' :: (MonadState BS.ByteString m) => m (U.Vector Char)
lineU' = do
  s <- line'
  pure $ U.fromListN (BS.length s) $ BS.unpack s

-- | Reads one line from the state and runs a pure parser for it.
{-# INLINE withLine' #-}
withLine' :: (MonadState BS.ByteString m) => State BS.ByteString a -> m a
withLine' f = evalState f <$> line'

-- * More

-- | Reads one line an unboxed vector.
{-# INLINE ints' #-}
ints' :: (MonadState BS.ByteString m) => m [Int]
ints' = unfoldr (BS.readInt . BS.dropSpace) <$> line'

-- | Reads one line an unboxed vector.
{-# INLINE intsU' #-}
intsU' :: (MonadState BS.ByteString m) => m (U.Vector Int)
intsU' = U.unfoldr (BS.readInt . BS.dropSpace) <$> line'

-- | Reads n values as an unboxed vector.
{-# INLINE intsN' #-}
intsN' :: (MonadState BS.ByteString m) => Int -> m (U.Vector Int)
intsN' n = U.replicateM n int'

-- | Reads one line as digits. TODO: one word might be better
{-# INLINE digitsU' #-}
digitsU' :: (MonadState BS.ByteString m) => m (U.Vector Int)
digitsU' = U.unfoldr (fmap (first digitToInt) . BS.uncons) <$> line'

