{-# LANGUAGE CPP #-}

-- Excluded on generation

module ToyLib.Macro where

-- Required when `DEBUG` macro is defined.
import Debug.Trace

-- When run as script, `dbg` expands to `traceShow`.
-- Otherwise it's an empty function.
#ifdef DEBUG
dbg :: Show a => a -> ()
dbg !x = do
  let !_ = traceShow x ()
  ()

dbgS :: String -> ()
dbgS !s = do
  let !_ = trace s ()
  ()

dbgSM :: (Monad m) => m String -> m ()
dbgSM !m = do
  !s <- m
  let !_ = trace s ()
  return ()

dbgId :: Show a => a -> a
dbgId !x = do
  let !_ = traceShow x ()
  x

note :: (Show s, Show a) => s -> a -> a
note !s !x = do
  let !_ = trace (show s ++ ": " ++ show x) ()
  x

dbgAssert :: Bool -> String -> ()
dbgAssert False !s = error $ "assertion failed!: " ++ s
dbgAssert True _ = ()

#else
dbg :: Show a => a -> ()
dbg _ = ()

dbgS :: String -> ()
dbgS _ = ()

dbgSM :: (Monad m) => m String -> m ()
dbgSM _ = return ()

dbgId :: Show a => a -> a
dbgId = id

note :: (Show s, Show a) => s -> a -> a
note _ !x = x

dbgAssert :: Bool -> a -> a
dbgAssert _ x = x
#endif
