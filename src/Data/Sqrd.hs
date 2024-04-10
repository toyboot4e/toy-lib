{-# LANGUAGE RecordWildCards #-}

-- | [Sqrt-decomposition](https://cp-algorithms.com/data_structures/sqrt_decomposition.html)
module Data.Sqrd where

import Control.Monad
import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U

type BlockIndex = Int

-- | [Sqrt-decomposition](https://cp-algorithms.com/data_structures/sqrt_decomposition.html)
data Sqrd b ret act m = Sqrd
  { blockLenSqrd :: !Int,
    blocksSqrd :: !(VM.MVector (PrimState m) b),
    -- Fold
    readFullSqrd :: !(BlockIndex -> m ret),
    readPartSqrd :: !(BlockIndex -> Int -> Int -> m ret),
    mergeSqrd :: !(ret -> ret -> m ret),
    -- Action
    actFullSqrd :: !(BlockIndex -> act -> m ()),
    actPartSqrd :: !(BlockIndex -> act -> Int -> Int -> m ())
  }

-- TODO: propagate before folding
foldSqrd :: (PrimMonad m) => Sqrd b ret act m -> Int -> Int -> m ret
foldSqrd Sqrd {..} l r = do
  let (!il, !remL) = l `divMod` blockLenSqrd
  let (!ir, !remR) = r `divMod` blockLenSqrd
  if il == ir
    then do
      readPartSqrd il remL remR
    else do
      !lx <- readPartSqrd il remL (blockLenSqrd - 1)
      !mx <- U.foldM' (\ !acc -> mergeSqrd acc <=< readFullSqrd) lx $ U.generate (ir - 1 - il) (+ (il + 1))
      !rx <- readPartSqrd ir 0 remR
      !ret <- mergeSqrd mx rx
      return ret

-- TODO: share the source code with @foldSqrd@.
actSqrd :: (PrimMonad m) => Sqrd b ret act m -> act -> Int -> Int -> m ()
actSqrd Sqrd {..} act l r = do
  let (!il, !remL) = l `divMod` blockLenSqrd
  let (!ir, !remR) = r `divMod` blockLenSqrd
  if il == ir
    then do
      actPartSqrd il act remL remR
    else do
      actPartSqrd il act remL (blockLenSqrd - 1)
      U.mapM_ (`actFullSqrd` act) $ U.generate (ir - 1 - il) (+ (il + 1))
      actPartSqrd ir act 0 remR

-- !readX <- newMutVar (0 :: Int)
-- !srd <- do
--   let !xss = V.unfoldrExactN nBlocks (U.splitAt blockLenSqrd) xs
--
--   -- no need of mutability though
--   !blocksSqrd <- V.unsafeThaw <=< V.forM xss $ \xs' -> do
--     !vec <- UM.replicate (G.length dict) (0 :: Int)
--     U.forM_ xs' $ \x -> do
--       UM.modify vec (+ x) (bindex dict x)
--     csum1D <$> U.unsafeFreeze vec
--
--   let {-# INLINE readFullSqrd #-}
--       readFullSqrd !iBlock = do
--         !x <- readMutVar readX
--         !csum <- VM.read blocksSqrd iBlock
--         let !i = maybe 0 (+ 1) $ bsearchL dict (<= x)
--         -- let !_ = dbg ("full", iBlock, x, csum, i)
--         return $ csum U.! i
--
--   let {-# INLINE readPartSqrd #-}
--       readPartSqrd !iBlock !l !r = do
--         let !xs' = xss V.! iBlock
--         !x <- readMutVar readX
--         -- let !_ = dbg ("part", iBlock, (l, r), x, xs')
--         return $ U.sum $ U.filter (<= x) $ U.take (r + 1 - l) $ U.drop l xs'
--
--   let {-# INLINE mergeSqrd #-}
--       mergeSqrd x y = return $ x + y
--
--   let {-# INLINE actFullSqrd #-}
--       actFullSqrd _ (_ :: Int) = return ()
--
--   let {-# INLINE actPartSqrd #-}
--       actPartSqrd _ (_ :: Int) _ _ = return ()
--
----   return Sqrd {..}
