-- {{{ toy-lib: https://github.com/toyboot4e/toy-lib
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-orphans #-}
{- ORMOLU_DISABLE -}
{-# LANGUAGE BlockArguments, CPP, DataKinds, DefaultSignatures, DerivingVia, LambdaCase, MagicHash, MultiWayIf, NumDecimals, PatternSynonyms, QuantifiedConstraints, RecordWildCards, StandaloneDeriving, StrictData, TypeFamilies, ViewPatterns #-}
import Control.Applicative;import Control.DeepSeq;import Control.Exception (assert);import Control.Monad;import Control.Monad.Fix;import Control.Monad.IO.Class;import Control.Monad.Primitive;import Control.Monad.ST;import Control.Monad.State.Class;import Control.Monad.Trans (MonadTrans, lift);import Control.Monad.Trans.Cont;import Control.Monad.Trans.Maybe;import Control.Monad.Trans.State.Strict (State, StateT, evalState, evalStateT, execState, execStateT, runState, runStateT);import Data.Bifunctor;import Data.Bits;import Data.Bool (bool);import Data.Char;import Data.Coerce;import Data.Either;import Data.Foldable;import Data.Function (on);import Data.Functor;import Data.Functor.Identity;import Data.IORef;import Data.Kind;import Data.List.Extra hiding (nubOn);import Data.Maybe;import Data.Ord;import Data.Primitive.MutVar;import Data.Proxy;import Data.STRef;import Data.Semigroup;import Data.Word;import Debug.Trace;import GHC.Exts (proxy#);import GHC.Float (int2Float);import GHC.Ix (unsafeIndex);import GHC.Stack (HasCallStack);import GHC.TypeLits;import System.Exit (exitSuccess);import System.IO;import System.Random;import System.Random.Stateful;import Text.Printf;import qualified Data.Ratio as Ratio;import Data.Array.IArray;import Data.Array.IO;import Data.Array.MArray;import Data.Array.ST;import Data.Array.Unboxed (UArray);import Data.Array.Unsafe;import qualified Data.Array as A;import Data.Bit;import qualified Data.ByteString.Builder as BSB;import qualified Data.ByteString.Char8 as BS;import qualified Data.ByteString.Unsafe as BSU;import Control.Monad.Extra hiding (loop);import Data.IORef.Extra;import Data.List.Extra hiding (merge);import Data.Tuple.Extra hiding (first, second);import Numeric.Extra;import Data.Bool.HT;import qualified Data.Ix.Enum as HT;import qualified Data.List.HT as HT;import qualified Data.Vector.Fusion.Bundle as FB;import qualified Data.Vector.Generic as G;import qualified Data.Vector.Generic.Mutable as GM;import qualified Data.Vector.Primitive as P;import qualified Data.Vector.Unboxed as U;import qualified Data.Vector.Unboxed.Base as U;import qualified Data.Vector.Unboxed.Mutable as UM;import qualified Data.Vector as V;import qualified Data.Vector.Mutable as VM;import qualified Data.Vector.Fusion.Bundle.Monadic as MB;import qualified Data.Vector.Fusion.Bundle.Size as MB;import qualified Data.Vector.Fusion.Stream.Monadic as MS;import qualified Data.Vector.Algorithms.Merge as VAM;import qualified Data.Vector.Algorithms.Intro as VAI;import qualified Data.Vector.Algorithms.Search as VAS;import qualified Data.IntMap.Strict as IM;import qualified Data.Map.Strict as M;import qualified Data.IntSet as IS;import qualified Data.Set as S;import qualified Data.Sequence as Seq;import qualified Data.Heap as H;import Data.Hashable;import qualified Data.HashMap.Strict as HM;import qualified Data.HashSet as HS;import qualified Test.QuickCheck as QC
{- ORMOLU_ENABLE -}

import ToyLib.Debug
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB
import Data.Graph.Sparse
import Data.Graph.Tree.Lca
import Data.Graph.Tree.TreeSG

{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

-- }}}

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !m) <- ints2'
  !es <- U.replicateM m ints11'
  q <- int'
  qs <- U.replicateM q ints11'

  let !gr = buildSG (0, n - 1) $ swapDupeU es

  let (!tree, !restVerts) = runST $ do
        uf <- newMUF n
        rest <- newBufferAsQueue (m - (n - 1))

        es' <- (`U.filterM` es) $ \(!v1, !v2) -> do
          b <- unifyMUF uf v1 v2
          unless b $ do
            pushBack rest v1
          return b

        let tree = buildSG (0, n - 1) $ swapDupeU es'
        (tree,) <$> unsafeFreezeBuffer rest

  let !bfs = V.map (vecIV . bfsSG gr) $ U.convert restVerts
  let !lcaCache = lcaCacheSG tree 0

  let fixed x
        | x < 0 = maxBound
        | otherwise = x

  let !res = (`U.map` qs) $ \(!v1, !v2) ->
        let !d1 = lcaLen lcaCache v1 v2
            !d2 = minimumOr maxBound $ (`V.map` bfs) $ \b -> fixed $ b U.! v1 + b U.! v2
         in min d1 d2

  putBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://atcoder.jp/contests/past202104-open/tasks/past202104_o
-- #LCA (lcaLen)
main :: IO ()
main = runIO solve
