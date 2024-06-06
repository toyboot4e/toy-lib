-- | Compatibilities.
module ToyLib.Compat where

import Control.Monad.Trans.State.Strict (StateT(..))

-- | Original: https://hackage.haskell.org/package/transformers-0.6.1.1/docs/src/Control.Monad.Trans.State.Strict.html#modifyM
modifyM :: (Monad m) => (s -> m s) -> StateT s m ()
modifyM f = StateT $ \ s -> do
    s' <- f s
    return ((), s')
{-# INLINE modifyM #-}

