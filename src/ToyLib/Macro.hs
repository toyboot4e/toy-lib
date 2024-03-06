{-# LANGUAGE CPP #-}

-- Excluded on generation

module ToyLib.Macro where

-- Required when `DEBUG` macro is defined.

#ifdef DEBUG
debug :: Bool
debug = True
#else
debug :: Bool
debug = False
#endif

