{-# LANGUAGE DefaultSignatures #-}

-- | Show with bytestring `Builder`.
--
-- It's good for both performance and monad transformers.
module ToyLib.ShowBSB where

-- TODO: foldMap vs foldMap'

import Control.Monad.IO.Class
import Data.Bool (bool)
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import System.IO (stdout)

{-# INLINE endlBSB #-}
endlBSB :: BSB.Builder
endlBSB = BSB.char7 '\n'

{-# INLINE putBSB #-}
putBSB :: (MonadIO m) => BSB.Builder -> m ()
putBSB = liftIO . BSB.hPutBuilder stdout

{-# INLINE putLnBSB #-}
putLnBSB :: (MonadIO m) => BSB.Builder -> m ()
putLnBSB = liftIO . BSB.hPutBuilder stdout . (<> endlBSB)

{-# INLINE wsBSB #-}
wsBSB :: BSB.Builder
wsBSB = BSB.char7 ' '

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
  showBSB (!a, !b, !c) = showBSB a <> BSB.string7 " " <> showBSB b <> BSB.string7 " " <> showBSB c

instance (ShowBSB a, ShowBSB b, ShowBSB c, ShowBSB d) => ShowBSB (a, b, c, d) where
  showBSB (!a, !b, !c, !d) = showBSB a <> BSB.string7 " " <> showBSB b <> BSB.string7 " " <> showBSB c <> BSB.string7 " " <> showBSB d

showLnBSB :: (ShowBSB a) => a -> BSB.Builder
showLnBSB = (<> endlBSB) . showBSB

printBSB :: (ShowBSB a, MonadIO m) => a -> m ()
printBSB = putBSB . showLnBSB

concatBSB :: (G.Vector v a, ShowBSB a) => v a -> BSB.Builder
concatBSB = G.foldMap showBSB

intersperseBSB :: (G.Vector v a, ShowBSB a) => BSB.Builder -> v a -> BSB.Builder
intersperseBSB = intersperseWithBSB showBSB

intersperseWithBSB :: (G.Vector v a) => (a -> BSB.Builder) -> BSB.Builder -> v a -> BSB.Builder
intersperseWithBSB showF del vec
  | G.null vec = mempty
  | otherwise = showF (G.head vec) <> G.foldMap ((del <>) . showF) (G.tail vec)

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
