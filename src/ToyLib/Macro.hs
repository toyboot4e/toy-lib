{-# LANGUAGE CPP #-}

-- Excluded on generation

module ToyLib.Macro where

-- Required when `DEBUG` macro is defined.
import Debug.Trace

-- When run as script, `dbg` expands to `traceShow`.
-- Otherwise it's an empty function.
#ifdef DEBUG
dbg :: Show a => a -> ()
dbg !x = let !_ = traceShow x () in ()

dbgS :: String -> ()
dbgS !s = let !_ = trace s () in ()

dbgId :: Show a => a -> a
dbgId !x = let !_ = traceShow x () in x

note :: (Show s, Show a) => s -> a -> a
note !s !x = let !_ = trace (show s ++ ": " ++ show x) () in x

dbgAssert :: Bool -> String -> ()
dbgAssert False !s = error $ "assertion failed!: " ++ s
dbgAssert True _ = ()

#else
dbg :: Show a => a -> ()
dbg _ = ()

dbgS :: String -> ()
dbgS _ = ()

dbgId :: Show a => a -> a
dbgId = id

note :: (Show s, Show a) => s -> a -> a
note _ !x = x

dbgAssert :: Bool -> a -> a
dbgAssert _ x = x
#endif
