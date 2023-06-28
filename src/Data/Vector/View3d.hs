{-# LANGUAGE BangPatterns #-}

module Data.Vector.View3d where

-- | 3D view to a vector

import Control.Monad.Primitive
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

type Index3d = (Int, Int, Int)

-- | @View3d (originalVector)
type View3d a = V.Vector (V.Vector a)

-- | Creates a 3D view to an immutable vector.
{-# INLINE view3dVG #-}
view3dVG :: (VG.Vector v a) => Index3d -> v a -> View3d (v a)
view3dVG (!d, !h, !w) !view = V.generate d (\id_ -> V.generate h (\ih -> VG.slice (id_ * (h * w) + ih * w) w view))

{-# INLINE withView3dVG #-}
withView3dVG :: (VG.Vector v a) => Index3d -> v a -> (v a, View3d (v a))
withView3dVG !hw !view = (view, view3dVG hw view)

-- | Creates a 3D view to a mutable vector.
{-# INLINE view3dVGM #-}
view3dVGM :: (VGM.MVector v a) => Index3d -> v s a -> View3d (v s a)
view3dVGM (!d, !h, !w) !view = V.generate d (\id_ -> V.generate h (\ih -> VGM.slice (id_ * (h * w) + ih * w) w view))

{-# INLINE withView3dVGM #-}
withView3dVGM :: (VGM.MVector v a) => Index3d -> v s a -> (v s a, View3d (v s a))
withView3dVGM !hw !view = (view, view3dVGM hw view)

-- | Creates a 3D mutable vector.
{-# INLINE new3dVGM #-}
new3dVGM :: (PrimMonad m, VGM.MVector v a) => Index3d -> a -> m (v (PrimState m) a, View3d (v (PrimState m) a))
new3dVGM (!d, !h, !w) !e0 = withView3dVGM (d, h, w) <$> VGM.replicate (d * h * w) e0

-- | Creates a 3D unboxed mutable vector.
{-# INLINE new3dVUM #-}
new3dVUM :: (PrimMonad m, VU.Unbox a) => Index3d -> a -> m (VUM.MVector (PrimState m) a, View3d (VUM.MVector (PrimState m) a))
new3dVUM = new3dVGM

-- | Creates a 3D boxed mutable vector.
{-# INLINE new3dVM #-}
new3dVM :: (PrimMonad m) => Index3d -> a -> m (VM.MVector (PrimState m) a, View3d (VM.MVector (PrimState m) a))
new3dVM = new3dVGM

-- | 3D immutable vector indexing.
{-# INLINE idx3d #-}
idx3d :: (VG.Vector v a) => View3d (v a) -> Index3d -> a
idx3d view (!d, !y, !x) = view V.! d V.! y VG.! x

{-# INLINE read3d #-}
read3d :: (PrimMonad m, VGM.MVector v a) => View3d (v (PrimState m) a) -> Index3d -> m a
read3d !view (!d, !y, !x) = VGM.read (view V.! d V.! y) x

{-# INLINE write3d #-}
write3d :: (PrimMonad m, VGM.MVector v a) => View3d (v (PrimState m) a) -> Index3d -> a -> m ()
write3d !view (!d, !y, !x) = VGM.write (view V.! d V.! y) x

{-# INLINE modify3d #-}
modify3d :: (PrimMonad m, VGM.MVector v a) => View3d (v (PrimState m) a) -> (a -> a) -> Index3d -> m ()
modify3d !view !alter (!d, !y, !x) = VGM.modify (view V.! d V.! y) alter x

{-# INLINE swap3d #-}
swap3d :: (PrimMonad m, VGM.MVector v a) => View3d (v (PrimState m) a) -> Index3d -> Index3d -> m ()
swap3d !view !ix1 !ix2 = do
  !v1 <- read3d view ix1
  !v2 <- read3d view ix2
  write3d view ix1 v2
  write3d view ix2 v1
