{-# LANGUAGE CPP #-}

-- Excluded on generation

module ToyLib.Macro where

-- When run as a stack script, `dbg` expands to `traceShow`.
-- Otherwise it's an empty function.
#ifdef DEBUG
dbg :: Show a => a -> ()
dbg !x = let !_ = traceShow x () in ()

dbgId :: Show a => a -> a
dbgId !x = let !_ = traceShow x () in x

note :: Show a => String -> a -> a
note !s !x = let !_ = trace (s ++ ": " ++ show x) () in x

dbgAssert :: Bool -> String -> ()
dbgAssert False !s = error $ "assertion failed!: " ++ s
dbgAssert True _ = ()

#else
dbg :: Show a => a -> ()
dbg _ = ()

dbgId :: Show a => a -> a
dbgId = id

note :: Show a => String -> a -> a
note _ !x = x

dbgAssert :: Bool -> a -> a
dbgAssert _ x = x
#endif

