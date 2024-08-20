{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | Semigroup action @*@ is an operator where \(s_2 * (s_1 * a) == (s_2 \diamond s_1) * a\) holds.
module Data.Core.SemigroupAction where

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Math.Stimes (stimes')

-- | Right semigroup aciton.
class SemigroupAction s a where
  -- | Right semigroup aciton
  sact :: s -> a -> a

-- | Performs `sact` with the binary lifting technique. Prefer the binary lifting module if the
-- cache takes effect.
sactTimes :: (Semigroup s, SemigroupAction s a) => Int -> s -> a -> a
sactTimes n0 s0 a0 = case compare n0 0 of
  LT -> errorWithoutStackTrace "sactTimes: zero or positive multiplier expected"
  EQ -> a0
  GT -> stimes' n0 s0 `sact` a0

instance (Semigroup a) => SemigroupAction a a where
  {-# INLINE sact #-}
  sact = (<>)

-- | `SemigroupAction` that does nothing.
newtype NoAction = NoAction ()
  deriving newtype (Eq, Ord, Show)

newtype instance UM.MVector s NoAction = MV_NoAction Int

newtype instance U.Vector NoAction = V_NoAction Int

instance U.Unbox NoAction

-- | Same as unit
instance GM.MVector UM.MVector NoAction where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}

  basicLength (MV_NoAction n) = n

  basicUnsafeSlice _ m (MV_NoAction _) = MV_NoAction m

  basicOverlaps _ _ = False

  basicUnsafeNew n = return (MV_NoAction n)

  -- Nothing to initialize
  basicInitialize _ = return ()

  basicUnsafeRead (MV_NoAction _) _ = return $ NoAction ()

  basicUnsafeWrite (MV_NoAction _) _ (NoAction ()) = return ()

  basicClear _ = return ()

  basicSet (MV_NoAction _) (NoAction ()) = return ()

  basicUnsafeCopy (MV_NoAction _) (MV_NoAction _) = return ()

  basicUnsafeGrow (MV_NoAction n) m = return $ MV_NoAction (n + m)

-- | Same as unit
instance G.Vector U.Vector NoAction where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (MV_NoAction n) = return $ V_NoAction n

  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (V_NoAction n) = return $ MV_NoAction n

  {-# INLINE basicLength #-}
  basicLength (V_NoAction n) = n

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice _ m (V_NoAction _) = V_NoAction m

  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (V_NoAction _) _ = return $ NoAction ()

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MV_NoAction _) (V_NoAction _) = return ()

  {-# INLINE elemseq #-}
  elemseq _ = seq
