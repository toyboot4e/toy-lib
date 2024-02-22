{-# LANGUAGE TypeFamilies #-}

-- | Unboxed 3D array. The `Unbox` implementation is from @Affine@ type in @cojna/iota@.
module Data.Instances.A3 where

import Control.Monad
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- | Unboxed 3D array.
data A3 a = A3 !a !a !a
  deriving (Eq, Show)

newtype instance UM.MVector s (A3 a) = MV_A3 (UM.MVector s a)

newtype instance U.Vector (A3 a) = V_A3 (U.Vector a)

instance (U.Unbox a) => U.Unbox (A3 a)

instance (U.Unbox a) => GM.MVector UM.MVector (A3 a) where
  basicLength (MV_A3 v) = GM.basicLength v `div` 3
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (MV_A3 v) = MV_A3 $ GM.basicUnsafeSlice (3 * i) (3 * n) v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_A3 v1) (MV_A3 v2) = GM.basicOverlaps v1 v2
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_A3 `liftM` GM.basicUnsafeNew (3 * n)
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_A3 v) = GM.basicInitialize v
  {-# INLINE basicInitialize #-}
  basicUnsafeRead (MV_A3 v) i = liftM3 A3 (GM.basicUnsafeRead v (3 * i)) (GM.basicUnsafeRead v (3 * i + 1)) (GM.basicUnsafeRead v (3 * i + 2))
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_A3 v) i (A3 x y z) = GM.basicUnsafeWrite v (3 * i) x >> GM.basicUnsafeWrite v (3 * i + 1) y >> GM.basicUnsafeWrite v (3 * i + 2) z
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_A3 v) = GM.basicClear v
  {-# INLINE basicClear #-}
  basicUnsafeCopy (MV_A3 v1) (MV_A3 v2) = GM.basicUnsafeCopy v1 v2
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_A3 v1) (MV_A3 v2) = GM.basicUnsafeMove v1 v2
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV_A3 v) n = MV_A3 `liftM` GM.basicUnsafeGrow v (3 * n)
  {-# INLINE basicUnsafeGrow #-}

instance (U.Unbox a) => G.Vector U.Vector (A3 a) where
  basicUnsafeFreeze (MV_A3 v) = V_A3 `liftM` G.basicUnsafeFreeze v
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_A3 v) = MV_A3 `liftM` G.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_A3 v) = G.basicLength v `div` 3
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (V_A3 v) = V_A3 $ G.basicUnsafeSlice (3 * i) (3 * n) v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_A3 v) i = liftM3 A3 (G.basicUnsafeIndexM v (3 * i)) (G.basicUnsafeIndexM v (3 * i + 1)) (G.basicUnsafeIndexM v (3 * i + 2))
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_A3 mv) (V_A3 v) = G.basicUnsafeCopy mv v
  elemseq _ = seq
  {-# INLINE elemseq #-}
