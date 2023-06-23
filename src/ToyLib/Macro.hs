{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

-- Excluded on generation

module ToyLib.Macro where

-- When run as a stack script, `dbg` expands to `traceShow`.
-- Otherwise it's an empty function.
#ifdef DEBUG
dbg :: Show a => a -> ()
dbg !x = let !_ = traceShow x () in ()

dbgAssert :: Bool -> String -> ()
dbgAssert False !s = error $ "assertion failed!: " ++ s
dbgAssert True _ = ()

#else
dbg :: Show a => a -> ()
dbg _ = ()

dbgAssert :: Bool -> a -> a
dbgAssert = flip const

#endif

