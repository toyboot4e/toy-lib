-- {{{ toy-lib: https://github.com/toyboot4e/toy-lib
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-orphans #-}
{- ORMOLU_DISABLE -}
{-# LANGUAGE BlockArguments, CPP, DataKinds, DefaultSignatures, DerivingVia, LambdaCase, MagicHash, MultiWayIf, NumDecimals, PatternSynonyms, QuantifiedConstraints, RecordWildCards, StandaloneDeriving, StrictData, TypeFamilies #-}
import Control.Applicative;import Control.DeepSeq;import Control.Exception (assert);import Control.Monad;import Control.Monad.Fix;import Control.Monad.IO.Class;import Control.Monad.Primitive;import Control.Monad.ST;import Control.Monad.State.Class;import Control.Monad.Trans (MonadTrans, lift);import Control.Monad.Trans.Cont;import Control.Monad.Trans.Maybe;import Control.Monad.Trans.State.Strict (State, StateT, evalState, evalStateT, execState, execStateT, runState, runStateT);import Data.Bifunctor;import Data.Bits;import Data.Bool (bool);import Data.Char;import Data.Coerce;import Data.Either;import Data.Foldable;import Data.Function (on);import Data.Functor;import Data.Functor.Identity;import Data.IORef;import Data.Kind;import Data.List.Extra hiding (nubOn);import Data.Maybe;import Data.Ord;import Data.Primitive.MutVar;import Data.Proxy;import Data.STRef;import Data.Semigroup;import Data.Word;import Debug.Trace;import GHC.Exts (proxy#);import GHC.Float (int2Float);import GHC.Ix (unsafeIndex);import GHC.Stack (HasCallStack);import GHC.TypeLits;import System.Exit (exitSuccess);import System.IO;import System.Random;import System.Random.Stateful;import Text.Printf;import qualified Data.Ratio as Ratio;import Data.Array.IArray;import Data.Array.IO;import Data.Array.MArray;import Data.Array.ST;import Data.Array.Unboxed (UArray);import Data.Array.Unsafe;import qualified Data.Array as A;import Data.Bit;import qualified Data.ByteString.Builder as BSB;import qualified Data.ByteString.Char8 as BS;import qualified Data.ByteString.Unsafe as BSU;import Control.Monad.Extra hiding (loop);import Data.IORef.Extra;import Data.List.Extra hiding (merge);import Data.Tuple.Extra hiding (first, second);import Numeric.Extra;import Data.Bool.HT;import qualified Data.Ix.Enum as HT;import qualified Data.List.HT as HT;import qualified Data.Vector.Fusion.Bundle as FB;import qualified Data.Vector.Generic as G;import qualified Data.Vector.Generic.Mutable as GM;import qualified Data.Vector.Primitive as P;import qualified Data.Vector.Unboxed as U;import qualified Data.Vector.Unboxed.Base as U;import qualified Data.Vector.Unboxed.Mutable as UM;import qualified Data.Vector as V;import qualified Data.Vector.Mutable as VM;import qualified Data.Vector.Fusion.Bundle.Monadic as MB;import qualified Data.Vector.Fusion.Bundle.Size as MB;import qualified Data.Vector.Fusion.Stream.Monadic as MS;import qualified Data.Vector.Algorithms.Merge as VAM;import qualified Data.Vector.Algorithms.Intro as VAI;import qualified Data.Vector.Algorithms.Search as VAS;import qualified Data.IntMap.Strict as IM;import qualified Data.Map.Strict as M;import qualified Data.IntSet as IS;import qualified Data.Set as S;import qualified Data.Sequence as Seq;import qualified Data.Heap as H;import Data.Hashable;import qualified Data.HashMap.Strict as HM;import qualified Data.HashSet as HS;import qualified Test.QuickCheck as QC
{- ORMOLU_ENABLE -}

-- {{{ toy-lib import 
import Data.Graph.MinCostFlow
import Data.Vector.IxVector
import ToyLib.Debug
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB

-- }}} toy-lib import 
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug = False
-- }}}

-- Bipassing answer
solve :: StateT BS.ByteString IO ()
solve = do
  (!h, !w) <- ints2'
  !mat <- getGrid' h w
  !ys <- U.replicateM h ints2'
  !xs <- U.replicateM w ints2'

  let !s0 = note "s0" $ U.sum (U.map snd ys) + U.sum (U.map snd xs)
  let !dys = U.map (uncurry (-)) ys
  let !dxs = U.map (uncurry (-)) xs

  -- Maximum weight matching in a bipartite graph.
  -- It's just a maximum cost flow problem, where we have weight and capacity as distinct values.
  let !targetFlow = h -- min h w
  let [!y0, !x0, !src, !sink, !nVerts] = scanl' (+) (0 :: Int) [h, w, 1, 1]

  -- edge: (v1, v2, capacity, cost)
  let es1 = U.generate h $ \iy -> (src, y0 + iy, 1, 0)
  let es2 = U.generate w $ \ix -> (x0 + ix, sink, 1, 0)
  let es3 = (`U.imapMaybe` vecIV mat) $ \i acc ->
        let (!y, !x) = i `divMod` w
            !dw = dys U.! y + dxs U.! x
            -- REMARK: remove negative edges
         in if acc == '0' || dw <= 0
              then Nothing
              else Just (y0 + y, x0 + x, 1, dw)

  -- because maximum flw is not always the best, we make up a bipassing edge:
  let bipass = U.singleton (src, sink, targetFlow, 0)
  let es' = bipass U.++ es1 U.++ es2 U.++ es3

  let (Max !delta, !_) = note "delta" $ maxCostFlow nVerts src sink targetFlow es'
  printBSB $ s0 + delta

-- verification-helper: PROBLEM https://atcoder.jp/contests/past202109-open/tasks/past202109_k
-- #min-cost-flow
main :: IO ()
main = runIO solve
