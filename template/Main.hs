{- ORMOLU_DISABLE -}

{-# LANGUAGE BlockArguments, DefaultSignatures, DerivingVia, LambdaCase, MultiWayIf, NumDecimals #-}
{-# LANGUAGE PatternSynonyms, QuantifiedConstraints, RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving, StrictData, TypeFamilies #-}

-- TODO: overloaded lists (not working for the union-find module?)
-- {-# LANGUAGE OverloadedLists, PatternSynonyms, QuantifiedConstraints, RecordWildCards #-}

{-# LANGUAGE CPP #-}

{- ORMOLU_ENABLE -}

-- {{{ Imports

module Template () where

import Control.Applicative
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
import Data.List.Extra hiding (nubOn)
import Data.Maybe
import Data.Ord
import Data.Primitive.MutVar
import Data.Proxy
import Data.STRef
import Data.Semigroup
import Data.Word
import Debug.Trace
import GHC.Exts
import GHC.Float (int2Float)
import GHC.Ix (unsafeIndex)
import GHC.Stack (HasCallStack)
import System.Exit (exitSuccess)
import System.IO
import System.Random
import System.Random.Stateful
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

main :: IO ()
main = do
  putStrLn "TODO"
