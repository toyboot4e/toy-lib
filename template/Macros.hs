#ifdef DEBUG
dbg :: Show a => a -> () ; dbg !x = let !_ = traceShow x () in () ; dbgAssert :: Bool -> String -> () ; dbgAssert False !s = error $ "assertion failed!: " ++ s ; dbgAssert True _ = () ; dbgS :: String -> () ; dbgS !s = let !_ = trace s () in () ;
#else
dbg :: Show a => a -> () ; dbg _ = () ; dbgAssert :: Bool -> a -> a ; dbgAssert = flip const ; dbgS :: String -> () ; dbgS _ = () ;
#endif
