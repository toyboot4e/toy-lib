{-# LANGUAGE BangPatterns #-}

module Data.Vector.View where

import Control.Monad.Primitive
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

-- | Creates a 2D view to an immutable vector.
{-# INLINE view2dVG #-}
view2dVG :: (VG.Vector v a) => (Int, Int) -> v a -> V.Vector (v a)
view2dVG (!h, !w) !vec = V.generate h (\i -> VG.slice (w * i) w vec)

{-# INLINE withView2dVG #-}
withView2dVG :: (VG.Vector v a) => (Int, Int) -> v a -> (v a, V.Vector (v a))
withView2dVG !hw !vec = (vec, view2dVG hw vec)

-- | Creates a 2D view to a mutable vector.
{-# INLINE view2dVGM #-}
view2dVGM :: (VGM.MVector v a) => (Int, Int) -> v s a -> V.Vector (v s a)
view2dVGM (!h, !w) !vec = V.generate h (\i -> VGM.slice (w * i) w vec)

{-# INLINE withView2dVGM #-}
withView2dVGM :: (VGM.MVector v a) => (Int, Int) -> v s a -> (v s a, V.Vector (v s a))
withView2dVGM !hw !vec = (vec, view2dVGM hw vec)

-- | Creates a 2D mutable vector.
{-# INLINE new2dVGM #-}
new2dVGM :: (PrimMonad m, VGM.MVector v a) => (Int, Int) -> a -> m (v (PrimState m) a, V.Vector (v (PrimState m) a))
new2dVGM (!h, !w) !e0 = withView2dVGM (h, w) <$> VGM.replicate (h * w) e0

-- | Creates a 2D unboxed mutable vector.
{-# INLINE new2dVUM #-}
new2dVUM :: (PrimMonad m, VU.Unbox a) => (Int, Int) -> a -> m (VUM.MVector (PrimState m) a, V.Vector (VUM.MVector (PrimState m) a))
new2dVUM = new2dVGM

-- | Creates a 2D boxed mutable vector.
{-# INLINE new2dVM #-}
new2dVM :: (PrimMonad m) => (Int, Int) -> a -> m (VM.MVector (PrimState m) a, V.Vector (VM.MVector (PrimState m) a))
new2dVM = new2dVGM

-- | 2D immutable vector indexing.
{-# INLINE idx2d #-}
idx2d :: (VG.Vector v a) => V.Vector (v a) -> (Int, Int) -> a
idx2d !vecs (!y, !x) = vecs V.! y VG.! x

{-# INLINE read2d #-}
read2d :: (PrimMonad m, VGM.MVector v a) => V.Vector (v (PrimState m) a) -> (Int, Int) -> m a
read2d !vec (!y, !x) = VGM.read (vec V.! y) x

{-# INLINE write2d #-}
write2d :: (PrimMonad m, VGM.MVector v a) => V.Vector (v (PrimState m) a) -> (Int, Int) -> a -> m ()
write2d !vec (!y, !x) = VGM.write (vec V.! y) x
