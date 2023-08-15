#!/usr/bin/env stack
{- stack script --resolver lts-21.6
--package array --package bytestring --package containers --package extra
--package hashable --package unordered-containers --package heaps --package utility-ht
--package vector --package vector-algorithms --package primitive
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
import Control.Arrow ((***))
import Control.Exception (assert)
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.Trans.State.Strict hiding (get)
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
import qualified Data.ByteString.Unsafe as BSU

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

type MyModInt = ModInt MyModulo

modInt :: Int -> MyModInt
modInt = ModInt . (`rem` typeInt (Proxy @MyModulo))

undef :: Int
undef = -1

-- }}}

main :: IO ()
main = do
  [n] <- ints
  !xs <- intsVU

  putStrLn "TODO"
