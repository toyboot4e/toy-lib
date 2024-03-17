-- | The great good procedual programming.
module ToyLib.Procedual where

orM' :: (Monad m) => Bool -> m Bool -> m Bool
orM' True _ = return True
orM' False b = b

andM' :: (Monad m) => Bool -> m Bool -> m Bool
andM' False _ = return False
andM' True b = b

