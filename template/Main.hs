#!/usr/bin/env stack
{- stack script --resolver lts-21.6
--package array --package bytestring --package containers --package extra
--package hashable --package unordered-containers --package heaps --package utility-ht
--package vector --package vector-th-unbox --package vector-algorithms --package primitive
--package transformers

--ghc-options "-D DEBUG"
-}

{- ORMOLU_DISABLE -}
{-# LANGUAGE BlockArguments, DefaultSignatures, DerivingVia, LambdaCase, MultiWayIf, NumDecimals #-}
{-# LANGUAGE QuantifiedConstraints, RecordWildCards, StandaloneDeriving, StrictData, TypeFamilies #-}

{-# LANGUAGE CPP, TemplateHaskell #-}
{- ORMOLU_ENABLE -}

-- {{{ Imports

module Template () where

import Control.Applicative
import Control.Exception (assert)
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.Trans.State.Strict
import Data.Bifunctor
import Data.Bits
import Data.Bool (bool)
import Data.Char
import Data.Coerce
import Data.Either
import Data.Foldable
import Data.Function (on)
import Data.Functor
import Data.Functor.Identity
import Data.IORef
import Data.List
import Data.Maybe
import Data.Ord
import Data.Proxy
import Data.STRef
import Data.Semigroup
import Data.Word
import Debug.Trace
import GHC.Exts
import GHC.Float (int2Float)
import System.Exit (exitSuccess)
import System.IO
import Text.Printf

{- ORMOLU_DISABLE -}

-- base
import qualified Data.Ratio as Ratio

-- array
import Data.Array.IArray
import Data.Array.IO
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed (UArray)
import Data.Array.Unsafe

import qualified Data.Array as A

-- bytestring
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS

-- extra
import Control.Monad.Extra hiding (loop) -- foldM, ..
import Data.IORef.Extra                  -- writeIORef'
import Data.List.Extra hiding (merge)    -- nubSort, ..
import Data.Tuple.Extra hiding (first, second)
import Numeric.Extra       -- showDP, intToFloat, ..

-- utility-ht
import Data.Bool.HT  -- if', ..
import qualified Data.Ix.Enum as HT
import qualified Data.List.HT as HT -- `groupBy`, but with adjacent elements

-- vector
import qualified Data.Vector.Fusion.Bundle as VFB
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Base as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import qualified Data.Vector.Fusion.Bundle.Monadic as MB
import qualified Data.Vector.Fusion.Bundle.Size    as MB
import qualified Data.Vector.Fusion.Stream.Monadic as MS

-- vector-algorithms
import qualified Data.Vector.Algorithms.Intro as VAI
import qualified Data.Vector.Algorithms.Search as VAS

-- vector-th-unbox
import Data.Vector.Unboxed.Deriving (derivingUnbox)

-- containers
import qualified Data.Graph as G
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.IntSet as IS
import qualified Data.Set as S
import qualified Data.Sequence as Seq

-- heaps
import qualified Data.Heap as H

-- unordered-containers
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

{- ORMOLU_ENABLE -}

-- }}}

-- {{ Adhoc

data MyModulo = MyModulo

instance TypeInt MyModulo where
  -- typeInt _ = 1_000_000_007
  typeInt _ = 998244353

derivingUnbox
  "HashSlice"
  [t|HashSlice MyModulo -> (Int, Int)|]
  [|\(HashSlice v l) -> (v, l)|]
  [|\(!v, !l) -> HashSlice v l|]

type MyModInt = ModInt MyModulo

modInt :: Int -> MyModInt
modInt = ModInt . (`rem` typeInt (Proxy @MyModulo))

undef :: Int
undef = -1

-- }}}

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

main :: IO ()
main = do
  [n] <- ints
  !xs <- intsVU

  putStrLn "TODO"
