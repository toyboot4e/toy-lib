{-# LANGUAGE TypeFamilies #-}

-- | Strict 2D tuple.
--
-- = WARNING: Not tested
module Data.Instances.T2 where

import Control.Monad
import Data.Bifunctor
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- | Unboxed 2D array packed in one array when stored in `U.Vector`.
data T2 a b = T2 !a !b
  deriving (Eq, Show)

instance Bifunctor T2 where
  {-# INLINE bimap #-}
  bimap f g (T2 a b) =
    let !a' = f a
        !b' = g b
     in T2 a' b'
  {-# INLINE first #-}
  first f (T2 a b) = let !a' = f a in T2 a' b
  {-# INLINE second #-}
  second g (T2 a b) = let !b' = g b in T2 a b'

instance (Ord a, Ord b) => Ord (T2 a b) where
  compare (T2 x y) (T2 x' y') = compare x x' <> compare y y'

----------------------------------------------------------------------------------------------------
-- Unbox
----------------------------------------------------------------------------------------------------

data instance UM.MVector s (T2 a b) = MV_T2 !(UM.MVector s a) !(UM.MVector s b)

data instance U.Vector (T2 a b) = V_T2 !(U.Vector a) !(U.Vector b)

instance (U.Unbox a, U.Unbox b) => U.Unbox (T2 a b)

instance (U.Unbox a, U.Unbox b) => GM.MVector UM.MVector (T2 a b) where
  {-# INLINE basicLength #-}
  basicLength (MV_T2 as _) = GM.basicLength as
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i n (MV_T2 as bs) = MV_T2 (GM.basicUnsafeSlice i n as) (GM.basicUnsafeSlice i n bs)
  {-# INLINE basicOverlaps #-}
  basicOverlaps (MV_T2 as1 bs1) (MV_T2 as2 bs2) = GM.basicOverlaps as1 as2 || GM.basicOverlaps bs1 bs2
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew n = liftM2 MV_T2 (GM.basicUnsafeNew n) (GM.basicUnsafeNew n)
  {-# INLINE basicInitialize #-}
  basicInitialize (MV_T2 as bs) = do
    GM.basicInitialize as
    GM.basicInitialize bs
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeReplicate n (T2 a b) = liftM2 MV_T2 (GM.basicUnsafeReplicate n a) (GM.basicUnsafeReplicate n b)
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MV_T2 as bs) i = liftM2 T2 (GM.basicUnsafeRead as i) (GM.basicUnsafeRead bs i)
  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MV_T2 as bs) i (T2 a b) = do
    GM.basicUnsafeWrite as i a
    GM.basicUnsafeWrite bs i b
  {-# INLINE basicClear #-}
  basicClear (MV_T2 as bs) = do
    GM.basicClear as
    GM.basicClear bs
  {-# INLINE basicSet #-}
  basicSet (MV_T2 as bs) (T2 a b) = do
    GM.basicSet as a
    GM.basicSet bs b
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MV_T2 as1 bs1) (MV_T2 as2 bs2) = do
    GM.basicUnsafeCopy as1 as2
    GM.basicUnsafeCopy bs1 bs2
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove (MV_T2 as1 bs1) (MV_T2 as2 bs2) = do
    GM.basicUnsafeMove as1 as2
    GM.basicUnsafeMove bs1 bs2
  {-# INLINE basicUnsafeGrow #-}
  basicUnsafeGrow (MV_T2 as bs) n = liftM2 MV_T2 (GM.basicUnsafeGrow as n) (GM.basicUnsafeGrow bs n)

instance (U.Unbox a, U.Unbox b) => G.Vector U.Vector (T2 a b) where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (MV_T2 as bs) = liftM2 V_T2 (G.basicUnsafeFreeze as) (G.basicUnsafeFreeze bs)
  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (V_T2 as bs) = liftM2 MV_T2 (G.basicUnsafeThaw as) (G.basicUnsafeThaw bs)
  {-# INLINE basicLength #-}
  basicLength (V_T2 as _) = G.basicLength as
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i n (V_T2 as bs) = V_T2 (G.basicUnsafeSlice i n as) (G.basicUnsafeSlice i n bs)
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (V_T2 as bs) i = liftM2 T2 (G.basicUnsafeIndexM as i) (G.basicUnsafeIndexM bs i)
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MV_T2 as1 bs1) (V_T2 as2 bs2) = do
    G.basicUnsafeCopy as1 as2
    G.basicUnsafeCopy bs1 bs2
  {-# INLINE elemseq #-}
  elemseq _ (T2 a b) = G.elemseq (undefined :: U.Vector a) a . G.elemseq (undefined :: U.Vector b) b
