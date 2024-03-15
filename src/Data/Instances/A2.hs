{-# LANGUAGE TypeFamilies #-}

-- | Unboxed 2D array stored in a packed 1-dimensional vector.
--
-- The `Unbox` implementation is from @Affine@ type in @cojna/iota@.
module Data.Instances.A2 where

import Data.Bits
import Control.Monad
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- | Unboxed 2D array packed in one array when stored in `U.Vector`.
data A2 a = A2 !a !a
  deriving (Eq, Show)

newtype instance UM.MVector s (A2 a) = MV_A2 (UM.MVector s a)

newtype instance U.Vector (A2 a) = V_A2 (U.Vector a)

instance (U.Unbox a) => U.Unbox (A2 a)

instance (U.Unbox a) => GM.MVector UM.MVector (A2 a) where
  basicLength (MV_A2 v) = unsafeShiftR (GM.basicLength v) 1 -- `div` 2
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (MV_A2 v) = MV_A2 $ GM.basicUnsafeSlice (2 * i) (2 * n) v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_A2 v1) (MV_A2 v2) = GM.basicOverlaps v1 v2
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_A2 `liftM` GM.basicUnsafeNew (2 * n)
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_A2 v) = GM.basicInitialize v
  {-# INLINE basicInitialize #-}
  basicUnsafeRead (MV_A2 v) i = liftM2 A2 (GM.basicUnsafeRead v (2 * i)) (GM.basicUnsafeRead v (2 * i + 1))
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_A2 v) i (A2 x y) = GM.basicUnsafeWrite v (2 * i) x >> GM.basicUnsafeWrite v (2 * i + 1) y
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_A2 v) = GM.basicClear v
  {-# INLINE basicClear #-}
  basicUnsafeCopy (MV_A2 v1) (MV_A2 v2) = GM.basicUnsafeCopy v1 v2
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_A2 v1) (MV_A2 v2) = GM.basicUnsafeMove v1 v2
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV_A2 v) n = MV_A2 `liftM` GM.basicUnsafeGrow v (2 * n)
  {-# INLINE basicUnsafeGrow #-}

instance (U.Unbox a) => G.Vector U.Vector (A2 a) where
  basicUnsafeFreeze (MV_A2 v) = V_A2 `liftM` G.basicUnsafeFreeze v
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_A2 v) = MV_A2 `liftM` G.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_A2 v) = unsafeShiftR (G.basicLength v) 1 -- `div` 2
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (V_A2 v) = V_A2 $ G.basicUnsafeSlice (2 * i) (2 * n) v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_A2 v) i = liftM2 A2 (G.basicUnsafeIndexM v (2 * i)) (G.basicUnsafeIndexM v (2 * i + 1))
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_A2 mv) (V_A2 v) = G.basicUnsafeCopy mv v
  elemseq _ = seq
  {-# INLINE elemseq #-}
