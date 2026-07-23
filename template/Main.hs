{- ORMOLU_DISABLE -}

{-# LANGUAGE BlockArguments, DataKinds, DefaultSignatures, DerivingVia, LambdaCase, MagicHash #-}
{-# LANGUAGE MultiWayIf, NumDecimals, PatternSynonyms, QuantifiedConstraints, RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving, StrictData, TypeFamilies, ViewPatterns #-}

-- {-# LANGUAGE OverloadedLists, PatternSynonyms, QuantifiedConstraints, RecordWildCards #-}

{-# LANGUAGE CPP #-}

{- ORMOLU_ENABLE -}

-- {{{ Imports

module Template () where

import Control.Applicative
import Control.DeepSeq
import Control.Exception (assert)
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.State.Class
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict (State, StateT (..), evalState, evalStateT, execState, execStateT, runState, runStateT)
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
import Data.Kind
import Data.List.Extra hiding (nubOn)
import Data.Maybe
import Data.Ord
import Data.Primitive
import Data.Primitive.MutVar
import Data.Proxy
import Data.STRef
import Data.Semigroup
import Data.Word
import Debug.Trace
import GHC.Exts (proxy#)
import GHC.Float (int2Float)
import GHC.Ix (unsafeIndex)
import GHC.Stack (HasCallStack)
import GHC.TypeLits
import System.Exit (exitSuccess)
import System.IO
import System.Random
import System.Random.Stateful
import Text.Printf
import Unsafe.Coerce

{- ORMOLU_DISABLE -}

-- base
import Data.Ratio

-- array
import Data.Array.IArray
import Data.Array.IO
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed (UArray)
import Data.Array.Unsafe

import Data.Array qualified as A

-- ac-library-hs
import AtCoder.Dsu qualified as Dsu
import AtCoder.Extra.Bisect qualified as B
import AtCoder.Extra.DsuMonoid qualified as DsuM
import AtCoder.Extra.Graph qualified as Gr
import AtCoder.Extra.Vector qualified as EV
import AtCoder.Extra.Vector.Prim qualified as EVP
import AtCoder.Extra.Bisect qualified as B
import AtCoder.Extra.Math qualified as EM
import AtCoder.Extra.HashMap qualified as EHM
import AtCoder.Extra.IntMap qualified as EIM
import AtCoder.Extra.IntSet qualified as EIS
import AtCoder.Extra.IntervalMap qualified as EIT
import AtCoder.Extra.Ix0
import AtCoder.Extra.Monoid.RangeAdd qualified as RangeAdd
import AtCoder.Extra.Monoid.RangeSet qualified as RangeSet
import AtCoder.Extra.Monoid.RollingHash qualified as RH
import AtCoder.Extra.Semigroup.Matrix qualified as Mat
import AtCoder.Extra.Semigroup.Permutation qualified as Permutation
import AtCoder.Extra.Tree qualified as Tr
import AtCoder.Extra.Tree.Hld qualified as Hld
import AtCoder.Extra.Tree.Lct qualified as Lct
import AtCoder.Extra.Tree.TreeMonoid qualified as Tm
import AtCoder.FenwickTree qualified as Ft
import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.MinHeap qualified as MH
import AtCoder.Internal.Queue qualified as Q
import AtCoder.LazySegTree qualified as LSeg
import AtCoder.ModInt qualified as MI
import AtCoder.SegTree qualified as Seg

-- bitvec
import Data.Bit

-- bytestring
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Unsafe qualified as BSU

-- extra
import Control.Monad.Extra hiding (loop) -- foldM, ..
import Data.IORef.Extra                  -- writeIORef'
import Data.List.Extra hiding (merge)    -- nubSort, ..
import Data.Tuple.Extra hiding (first, second)
import Numeric.Extra       -- showDP, intToFloat, ..

-- utility-ht
import Data.Bool.HT  -- if', ..
import Data.Ix.Enum qualified as HT
import Data.List.HT qualified as HT -- `groupBy`, but with adjacent elements

-- vector
import Data.Vector.Fusion.Bundle qualified as FB
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Primitive qualified as P
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Base qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM

import Data.Vector.Fusion.Bundle.Monadic qualified as MB
import Data.Vector.Fusion.Bundle.Size qualified    as MB
import Data.Vector.Fusion.Stream.Monadic qualified as MS

-- vector-algorithms
import Data.Vector.Algorithms.Merge qualified as VAM
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Algorithms.Radix qualified as VAR
import Data.Vector.Algorithms.Search qualified as VAS

-- containers
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict qualified as M
import Data.IntSet qualified as IS
import Data.Set qualified as S
import Data.Sequence qualified as Seq

-- heaps
import Data.Heap qualified as H

-- unordered-containers
import Data.Hashable
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS

-- quickcheck
import Test.QuickCheck qualified as QC

{- ORMOLU_ENABLE -}

-- }}}

main :: IO ()
main = do
  putStrLn "TODO"
