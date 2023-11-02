{-# LANGUAGE LambdaCase #-}

-- | 01-BFS
module ToyLib.BFS01 where

import Control.Monad
import Control.Monad.Fix
import Data.Bool (bool)
import Data.Buffer
import Data.Ix
import Data.Tuple.Extra (both)
import Data.Vector.IxVector
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import ToyLib.Prelude (repM_)
import GHC.Stack (HasCallStack)

-- 01-BFS: zero cost with same direction.
bfs01_grid4_typical043 :: HasCallStack => IxUVector (Int, Int) Bool -> (Int, Int) -> IxUVector (Int, Int, Int) Int
bfs01_grid4_typical043 !isBlock !start = IxVector boundsExt $ U.create $ do
  -- vec @! (dir, y, x)
  !vec <- IxVector boundsExt <$> UM.replicate (4 * nVerts) undef

  let !redundantSpace = 0
  !deque <- newBufferAsDeque (redundantSpace + 4 * nVerts)
  repM_ 0 3 $ \iDir -> do
    let !vExt = (iDir, fst start, snd start)
    pushFront deque (0 :: Int, vExt)
    writeIV vec vExt (0 :: Int)

  let extract !w0 vExt0@(!iDir0, !y0, !x0) = do
        !wReserved0 <- readIV vec vExt0
        when (w0 == wReserved0) $ do
          U.iforM_ dirs $ \iDir (!dy, !dx) -> do
            let !v = (y0 + dy, x0 + dx)
            when (inRange bounds_ v && not (isBlock @! v)) $ do
              let !w = bool (w0 + 1) w0 (iDir == iDir0)
              let !vExt = (iDir, y0 + dy, x0 + dx)
              !wReserved <- readIV vec vExt
              when (wReserved == undef || w < wReserved) $ do
                writeIV vec vExt w
                if iDir == iDir0
                  then pushFront deque (w, vExt)
                  else pushBack deque (w, vExt)

  -- generic BFS = pop loop
  fix $ \loop ->
    popFront deque >>= \case
      Nothing -> return ()
      Just (!w, !v) -> do
        extract w v
        loop

  return $ vecIV vec
  where
    !undef = -1 :: Int
    (!height, !width) = both succ . snd $ boundsIV isBlock
    !bounds_ = boundsIV isBlock
    !boundsExt = ((0, 0, 0), (3, height - 1, width - 1))
    !nVerts = rangeSize bounds_
    !dirs = U.fromList [(0, 1), (0, -1), (1, 0), (-1, 0)]
