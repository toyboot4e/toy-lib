{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Compatibilities.
module ToyLib.Compat where

import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Control.Monad.Trans.State.Strict (StateT (..))
import Data.Bits ((.>>.))
import Data.Coerce
import Data.Vector qualified as V
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Mutable qualified as VM
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

-- | Original: https://hackage.haskell.org/package/transformers-0.6.1.1/docs/src/Control.Monad.Trans.State.Strict.html#modifyM
modifyM :: (Monad m) => (s -> m s) -> StateT s m ()
modifyM f = StateT $ \s -> do
  s' <- f s
  pure ((), s')
{-# INLINE modifyM #-}
