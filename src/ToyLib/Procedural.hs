{-# LANGUAGE LambdaCase #-}

-- | The great good procedual programming.
module ToyLib.Procedural where

import Control.Monad.Fix
import qualified Data.Vector.Generic as G

or1 :: (Monad m) => Bool -> m Bool -> m Bool
or1 True _ = return True
or1 False b = b

and1 :: (Monad m) => Bool -> m Bool -> m Bool
and1 False _ = return False
and1 True b = b

or2 :: (Monad m) => m Bool -> m Bool -> m Bool
or2 x y = do
  b <- x
  or1 b y

and2 :: (Monad m) => m Bool -> m Bool -> m Bool
and2 x y = do
  b <- x
  and1 b y

-- | Pruning.
anyMG :: (Monad m, G.Vector v a) => (a -> m Bool) -> v a -> m Bool
anyMG !f = fix $ \loop acc -> case G.uncons acc of
    Nothing -> return False
    Just (!x, !acc') -> do
      f x >>= \case
        True -> return True
        False -> loop acc'

-- | Pruning.
allMG :: (Monad m, G.Vector v a) => (a -> m Bool) -> v a -> m Bool
allMG !f = fix $ \loop acc -> case G.uncons acc of
    Nothing -> return True
    Just (!x, !acc') -> do
      f x >>= \case
        False -> return False
        True -> loop acc'

