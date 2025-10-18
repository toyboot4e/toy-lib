{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Compatibilities.
module ToyLib.Compat where

import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Control.Monad.Trans.State.Strict (StateT (..))
import Data.Bits ((.>>.))
import Data.Coerce
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- | Original: https://hackage.haskell.org/package/transformers-0.6.1.1/docs/src/Control.Monad.Trans.State.Strict.html#modifyM
modifyM :: (Monad m) => (s -> m s) -> StateT s m ()
modifyM f = StateT $ \s -> do
  s' <- f s
  pure ((), s')
{-# INLINE modifyM #-}
