#ifdef DEBUG
dbg :: Show a => a -> () ; dbg !x = let !_ = traceShow x () in () ; dbgAssert :: Bool -> String -> () ; dbgAssert False !s = error $ "assertion failed!: " ++ s ; dbgAssert True _ = () ; dbgS :: String -> () ; dbgS !s = let !_ = trace s () in () ; dbgId :: Show a => a -> a ; dbgId !x = let !_ = traceShow x () in x ; note :: Show a => String -> a -> a ; note !s !x = let !_ = trace (s ++ show x) () in x ;
#else
dbg :: Show a => a -> () ; dbg _ = () ; dbgAssert :: Bool -> a -> a ; dbgAssert = flip const ; dbgS :: String -> () ; dbgS _ = () ; dbgId :: Show a => a -> a ; dbgId = id ; note :: Show a => String -> a -> a ; note _ !x = x ;
#endif
