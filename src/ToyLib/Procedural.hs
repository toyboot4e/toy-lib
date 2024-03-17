-- | The great good procedual programming.
module ToyLib.Procedual where

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

