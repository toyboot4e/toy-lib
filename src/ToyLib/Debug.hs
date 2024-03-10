-- | Debug utilities
module ToyLib.Debug where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.UnionFind.Mutable
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import Debug.Trace
import ToyLib.Macro

-- When run as script, `dbg` expands to `traceShow`.
-- Otherwise it's an empty function.
dbg :: (Show a) => a -> ()
dbg x
  | debug = let !_ = traceShow x () in ()
  | otherwise = ()

dbgS :: String -> ()
dbgS s
  | debug = let !_ = trace s () in ()
  | otherwise = ()

dbgSM :: (Monad m) => m String -> m ()
dbgSM m
  | debug = do
      !s <- m
      let !_ = trace s ()
      return ()
  | otherwise = return ()

dbgId :: (Show a) => a -> a
dbgId x
  | debug = let !_ = traceShow x () in x
  | otherwise = x

note :: (Show s, Show a) => s -> a -> a
note s x
  | debug = let !_ = trace (show s ++ ": " ++ show x) () in x
  | otherwise = x

dbgAssert :: Bool -> String -> ()
dbgAssert b s
  | debug && not b = error $ "assertion failed!: " ++ s
  | otherwise = ()

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
      return ()
  | otherwise = return ()

-- FIXME: why such a redundant contraint is required?
-- | Shows the Union-Find vertices.
dbgUF :: (PrimMonad m, Show (U.Vector MUFNode)) => MUnionFind (PrimState m) -> m ()
dbgUF (MUnionFind vec) = dbgVec vec
