{-# LANGUAGE BangPatterns #-}

module Data.Vector.View2d where

import Control.Monad.Primitive
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

type Index2d = (Int, Int)

-- | @View2d (originalVector)
type View2d = V.Vector

-- | Creates a 2D view to an immutable vector.
{-# INLINE view2dVG #-}
view2dVG :: (VG.Vector v a) => Index2d -> v a -> View2d (v a)
view2dVG (!h, !w) !view = V.generate h (\ih -> VG.slice (ih * w) w view)

{-# INLINE withView2dVG #-}
withView2dVG :: (VG.Vector v a) => Index2d -> v a -> (v a, View2d (v a))
withView2dVG !hw !view = (view, view2dVG hw view)

-- | Creates a 2D view to a mutable vector.
{-# INLINE view2dVGM #-}
view2dVGM :: (VGM.MVector v a) => Index2d -> v s a -> View2d (v s a)
view2dVGM (!h, !w) !view = V.generate h (\ih -> VGM.slice (ih * w) w view)

{-# INLINE withView2dVGM #-}
withView2dVGM :: (VGM.MVector v a) => Index2d -> v s a -> (v s a, View2d (v s a))
withView2dVGM !hw !view = (view, view2dVGM hw view)

-- | Creates a 2D mutable vector.
{-# INLINE new2dVGM #-}
new2dVGM :: (PrimMonad m, VGM.MVector v a) => Index2d -> a -> m (v (PrimState m) a, View2d (v (PrimState m) a))
new2dVGM (!h, !w) !e0 = withView2dVGM (h, w) <$> VGM.replicate (h * w) e0

-- | Creates a 2D unboxed mutable vector.
{-# INLINE new2dVUM #-}
new2dVUM :: (PrimMonad m, VU.Unbox a) => Index2d -> a -> m (VUM.MVector (PrimState m) a, View2d (VUM.MVector (PrimState m) a))
new2dVUM = new2dVGM

-- | Creates a 2D boxed mutable vector.
{-# INLINE new2dVM #-}
new2dVM :: (PrimMonad m) => Index2d -> a -> m (VM.MVector (PrimState m) a, View2d (VM.MVector (PrimState m) a))
new2dVM = new2dVGM

-- | 2D immutable vector indexing.
{-# INLINE idx2d #-}
idx2d :: (VG.Vector v a) => View2d (v a) -> Index2d -> a
idx2d view (!y, !x) = view V.! y VG.! x

{-# INLINE read2d #-}
read2d :: (PrimMonad m, VGM.MVector v a) => View2d (v (PrimState m) a) -> Index2d -> m a
read2d !view (!y, !x) = VGM.read (view V.! y) x

{-# INLINE write2d #-}
write2d :: (PrimMonad m, VGM.MVector v a) => View2d (v (PrimState m) a) -> Index2d -> a -> m ()
write2d !view (!y, !x) = VGM.write (view V.! y) x

{-# INLINE modify2d #-}
modify2d :: (PrimMonad m, VGM.MVector v a) => View2d (v (PrimState m) a) -> (a -> a) -> Index2d -> m ()
modify2d !view !alter (!y, !x) = VGM.modify (view V.! y) alter x
