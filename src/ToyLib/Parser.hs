-- | `StateT` -based parser.
module ToyLib.Parser where

import Control.Monad.State.Class
import Control.Monad.Trans.State.Strict (State, StateT, evalState, evalStateT)
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
{-# INLINE intP #-}
intP :: (MonadState BS.ByteString m) => m Int
intP = state $ fromJust . BS.readInt . BS.dropSpace

-- | Parses an `Int` and substracts one.
{-# INLINE int1P #-}
int1P :: (MonadState BS.ByteString m) => m Int
int1P = subtract 1 <$> intP

{-# INLINE charP #-}
charP :: (MonadState BS.ByteString m) => m Char
charP = state $ fromJust . BS.uncons . BS.dropSpace

{-# INLINE wordP #-}
wordP :: (MonadState BS.ByteString m) => m BS.ByteString
wordP = state $ BS.break isSpace . BS.dropSpace

-- | Parsers an `Int` and converts it into a `Double`.
doubleP :: (MonadState BS.ByteString m) => m Double
doubleP = read . BS.unpack <$> wordP

-- * Tuples

{-# INLINE ints2P #-}
ints2P :: (MonadState BS.ByteString m) => m (Int, Int)
ints2P = (,) <$> intP <*> intP

{-# INLINE ints11P #-}
ints11P :: (MonadState BS.ByteString m) => m (Int, Int)
ints11P = (,) <$> int1P <*> int1P

{-# INLINE ints3P #-}
ints3P :: (MonadState BS.ByteString m) => m (Int, Int, Int)
ints3P = (,,) <$> intP <*> intP <*> intP

{-# INLINE ints110P #-}
ints110P :: (MonadState BS.ByteString m) => m (Int, Int, Int)
ints110P = (,,) <$> int1P <*> int1P <*> intP

{-# INLINE ints011 #-}
ints011 :: (MonadState BS.ByteString m) => m (Int, Int, Int)
ints011 = (,,) <$> intP <*> int1P <*> int1P

{-# INLINE ints111 #-}
ints111 :: (MonadState BS.ByteString m) => m (Int, Int, Int)
ints111 = (,,) <$> int1P <*> int1P <*> int1P

{-# INLINE ints4P #-}
ints4P :: (MonadState BS.ByteString m) => m (Int, Int, Int, Int)
ints4P = (,,,) <$> intP <*> intP <*> intP <*> intP

{-# INLINE ints5P #-}
ints5P :: (MonadState BS.ByteString m) => m (Int, Int, Int, Int, Int)
ints5P = (,,,,) <$> intP <*> intP <*> intP <*> intP <*> intP

{-# INLINE ints6P #-}
ints6P :: (MonadState BS.ByteString m) => m (Int, Int, Int, Int, Int, Int)
ints6P = (,,,,,) <$> intP <*> intP <*> intP <*> intP <*> intP <*> intP

-- * Readers

-- | Reads one line from the state. FIXME: It ignores empty lines.
{-# INLINE lineP #-}
lineP :: (MonadState BS.ByteString m) => m BS.ByteString
lineP = state $ BS.span (/= '\n') . BS.dropSpace

-- | Reads one line from the state.
{-# INLINE lineUP #-}
lineUP :: (MonadState BS.ByteString m) => m (U.Vector Char)
lineUP = do
  s <- lineP
  pure $ U.fromListN (BS.length s) $ BS.unpack s

-- | Reads one line from the state and runs a pure parser for it.
{-# INLINE withLine #-}
withLine :: (MonadState BS.ByteString m) => State BS.ByteString a -> m a
withLine f = evalState f <$> lineP

-- * More

-- | Reads one line an unboxed vector.
{-# INLINE intListP #-}
intListP :: (MonadState BS.ByteString m) => m [Int]
intListP = unfoldr (BS.readInt . BS.dropSpace) <$> lineP

-- | Reads one line an unboxed vector.
{-# INLINE intsP #-}
intsP :: (MonadState BS.ByteString m) => m (U.Vector Int)
intsP = U.unfoldr (BS.readInt . BS.dropSpace) <$> lineP

-- | Reads n values as an unboxed vector.
{-# INLINE intsNP #-}
intsNP :: (MonadState BS.ByteString m) => Int -> m (U.Vector Int)
intsNP n = U.replicateM n intP

-- | Reads one line as digits. TODO: one word might be better
{-# INLINE digitsP #-}
digitsP :: (MonadState BS.ByteString m) => m (U.Vector Int)
digitsP = U.unfoldr (fmap (first digitToInt) . BS.uncons) <$> lineP
