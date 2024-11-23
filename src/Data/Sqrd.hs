{-# LANGUAGE RecordWildCards #-}

-- | [Sqrt-decomposition](https://cp-algorithms.com/data_structures/sqrt_decomposition.html).
-- It often solves Q queries in \(O(\sqrt N F)\).
module Data.Sqrd where

import Control.Monad
import Control.Monad.Primitive (PrimMonad)
import qualified Data.Vector.Unboxed as U

type BlockIndex = Int

-- | [Sqrt-decomposition](https://cp-algorithms.com/data_structures/sqrt_decomposition.html)
--
-- = Typical problems
-- - [ABC 339 G - Smaller Sum](https://atcoder.jp/contests/abc339/tasks/abc339_g) (easy)
--
-- TODO: Add read parameter?
data Sqrd b ret act m = Sqrd
  { blockLenSqrd :: {-# UNPACK #-} !Int,
    -- Fold
    readFullSqrd :: !(BlockIndex -> m ret),
    readPartSqrd :: !(BlockIndex -> Int -> Int -> m ret),
    mergeSqrd :: !(ret -> ret -> m ret),
    -- Action
    actFullSqrd :: !(BlockIndex -> act -> m ()),
    actPartSqrd :: !(BlockIndex -> act -> Int -> Int -> m ())
  }

-- TODO: propagate before folding

-- | \(O(\sqrt N f)\)
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
      pure ret

-- | \(O(\sqrt N f)\)
actSqrd :: (PrimMonad m) => Sqrd b ret act m -> act -> Int -> Int -> m ()
-- TODO: share the source code with @foldSqrd@.
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

-- -- O(N \log N)
-- let !xss = V.unfoldrExactN nBlocks (U.splitAt blockLenSqrd) xs
-- !blocks <- V.forM xss $ \xs' -> do
--   !vec <- UM.replicate (G.length dict) (0 :: Int)
--   U.forM_ xs' $ \x -> do
--     UM.modify vec (+ x) (bindex dict x)
--   csum1D <$> U.unsafeFreeze vec
--
-- !srd <- do
--   let {-# INLINE readFullSqrd #-}
--       readFullSqrd !iBlock = do
--         !x <- get
--         let !csum = blocks V.! iBlock :: U.Vector Int
--         let !i = maybe 0 (+ 1) $ bsearchL dict (<= x)
--         pure $ csum G.! i
--
--   let {-# INLINE readPartSqrd #-}
--       -- @l@ and @r@ are in global coordinates
--       readPartSqrd !iBlock !l !r = do
--         let !xs' = xss V.! iBlock
--         !x <- get
--         pure $ U.sum $ U.filter (<= x) $ U.take (r + 1 - l) $ U.drop l xs'
--
--   let {-# INLINE mergeSqrd #-}
--       mergeSqrd x y = pure $ x + y
--
--   let {-# INLINE actFullSqrd #-}
--       actFullSqrd _iBlock (_ :: Int) = pure ()
--
--   let {-# INLINE actPartSqrd #-}
--       -- @l@ and @r@ are in local coordinates in the block
--       readPartSqrd !iBlock !l !r = do
--       actPartSqrd _iBlock (_ :: Int) _lLocal _rLocal = pure ()
--
--   pure Sqrd {..}
