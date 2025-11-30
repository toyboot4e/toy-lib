-- | Grid parsers.
module ToyLib.Parser.Grid where

import Control.Monad.Primitive (PrimMonad)
import Control.Monad.State.Class
import qualified Data.ByteString.Char8 as BS
import Data.Ix
import qualified Data.Vector.Generic.Mutable as GM
import Data.Vector.IxVector
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import ToyLib.Parser

-- | \(O(HW)\) Reads next @h * w@ elements as matrix of type @a@.
--
-- = Example
--
-- Input:
--
-- @
-- 1 2 3
-- 4 5 6
-- @
--
-- Parsed result:
--
-- @
-- 1 2 3
-- 4 5 6
-- @
matP :: (MonadState BS.ByteString m) => Int -> Int -> m (IxVector (Int, Int) (U.Vector Int))
matP !h !w = IxVector ((0, 0), (h - 1, w - 1)) <$> U.replicateM (h * w) intP

-- | \(O(HW)\) Reads next @h * w@ elements as a char-based grid.
--
-- = Example
--
-- Input:
--
-- @
-- abc
-- def
-- @
--
-- Parsed result:
--
-- @
-- a b c
-- d e f
-- @
gridP :: (MonadState BS.ByteString m) => Int -> Int -> m (IxUVector (Int, Int) Char)
gridP !h !w = IxVector ((0, 0), (h - 1, w - 1)) <$> U.replicateM (h * w) charP

-- | \(O(HW)\) Gets diagnoal matrix input.
--
-- = Example
--
-- Input:
--
-- @
-- 4 8 5
-- 6 8
-- 3
-- @
--
-- Parsed result:
--
-- @
-- 0 4 8 5
-- 4 0 6 8
-- 8 6 0 3
-- 5 8 3 0
-- @
diagMatP :: (PrimMonad m, MonadState BS.ByteString m) => Int -> m (IxUVector (Int, Int) Int)
diagMatP !n = fmap (IxVector bnd) $ do
  !vec <- UM.replicate (n * n) (0 :: Int)
  U.forM_ (U.generate (n - 1) id) $ \y -> do
    !ws <- intsP
    U.iforM_ ws $ \i dw -> do
      let !x = y + i + 1
      GM.write vec (index bnd (y, x)) dw
      GM.write vec (index bnd (x, y)) dw
  U.unsafeFreeze vec
  where
    !bnd = ((0, 0), (n - 1, n - 1))
