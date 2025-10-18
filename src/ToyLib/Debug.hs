-- | Debug utilities
module ToyLib.Debug where

import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector.Generic as G
import Debug.Trace
import GHC.Stack (HasCallStack)
import ToyLib.Macro

-- When run as script, `dbg` expands to `traceShow`.
-- Otherwise it's an empty function.
dbg :: (Show a) => a -> ()
dbg x
  | debug = let !_ = traceShow x () in ()
  | otherwise = ()

dbgM :: (Monad m, Show a) => m a -> m ()
dbgM m
  | debug = do
      !s <- m
      let !_ = traceShow s ()
      pure ()
  | otherwise = pure ()

dbgS :: String -> ()
dbgS s
  | debug = let !_ = trace s () in ()
  | otherwise = ()

dbgSM :: (Monad m) => m String -> m ()
dbgSM m
  | debug = do
      !s <- m
      let !_ = trace s ()
      pure ()
  | otherwise = pure ()

dbgId :: (Show a) => a -> a
dbgId x
  | debug = let !_ = traceShow x () in x
  | otherwise = x

note :: (Show s, Show a) => s -> a -> a
note s x
  | debug = let !_ = trace (show s ++ ": " ++ show x) () in x
  | otherwise = x

dbgAssert :: (HasCallStack) => Bool -> String -> ()
dbgAssert b s
  | debug && not b = error $ "assertion failed!: " ++ s
  | otherwise = ()

asserted :: (HasCallStack) => Bool -> a -> a
asserted b x
  | debug && not b = error "assertion failed!"
  | otherwise = x

-- | `$` with `dbgId`
($$) :: (Show a) => (a -> b) -> a -> b
($$) lhs rhs = lhs (dbgId rhs)

infixr 0 $$

-- | `.` with `dbgId`. TODO: Working?
(.$) :: (Show b) => (b -> c) -> (a -> b) -> a -> c
g .$ f = \a -> let !b = dbgId (f a) in g b

infixr 9 .$

-- | Shows the mutable vector.
dbgVec :: (Show (v a), G.Vector v a, PrimMonad m) => (G.Mutable v) (PrimState m) a -> m ()
dbgVec vec
  | debug = do
      !xs' <- G.unsafeFreeze vec
      let !_ = dbg xs'
      pure ()
  | otherwise = pure ()
