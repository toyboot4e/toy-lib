-- | Typical DP utilities

module ToyLib.DP where

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM

-- | Variant of `VU.constructN`.
constructFor :: (VU.Unbox a, VU.Unbox b) => a -> VU.Vector b -> (VU.Vector a -> b -> a) -> VU.Vector a
constructFor !x0 !input !f = VU.create $ do
  !vec <- VUM.unsafeNew (VU.length input + 1)
  VUM.unsafeWrite vec 0 x0

  flip VU.imapM_ input $ \lenS1 x -> do
    !vec' <- VU.take (succ lenS1) <$> VU.unsafeFreeze vec
    VUM.unsafeWrite vec (succ lenS1) $! f vec' x

  return vec

-- | @relaxMany !f !vec0 !input !expander@ ~ @VG.accumulate f vec0 $ VG.concatMap expander input@
relaxMany :: (VG.Vector v a, VG.Vector v (Int, a), VG.Vector v b) => (a -> a -> a) -> v a -> v b -> (b -> v (Int, a)) -> v a
relaxMany !relax !vec0 !input !expander = VG.create $ do
  !vec <- VG.unsafeThaw vec0

  VG.forM_ input $ \x -> do
    VG.forM_ (expander x) $ \(!i, !x') -> do
      VGM.modify vec (`relax` x') i

  return vec

-- | Monoid variant of `relaxMany`
relaxMany' :: (Monoid m, VU.Unbox m, VU.Unbox a) => VU.Vector m -> VU.Vector a -> (a -> VU.Vector (Int, m)) -> VU.Vector m
relaxMany' !vec0 !input !expander = VU.create $ do
  !vec <- VU.unsafeThaw vec0

  VU.forM_ input $ \x -> do
    VU.forM_ (expander x) $ \(!i, !x') -> do
      VUM.modify vec (<> x') i

  return vec

