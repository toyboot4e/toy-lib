{-# LANGUAGE RecordWildCards #-}

-- | \(O(V + E)\) 2-satisfiability problem.
--
-- 2-satisfiablity problem is about finding one possible assignment to boolean variables
-- \(\{x_i\}_i\) under a conjunciton of or clauses: \(\vee_i \{x_{i,1} \wedge x_{i,2}\}_i\).
--
-- = Example
--
-- Use `twoSat` for denoting and solving the problem:
--
-- @
-- let !nClauses = (2 * n * pred n)
-- let !res = twoSat n nClauses $ \tsb -> do
--       forM_ [0 .. n - 1] $ \v1 -> do
--         let (!x1, !y1) = xys U.! v1
--         forM_ [v1 + 1 .. n - 1] $ \v2 -> do
--           let (!x2, !y2) = xys U.! v2
--           when (abs (x1 - x2) < d) $ do
--             addOrTSB tsb (F v1) (F v2)
--           when (abs (x1 - y2) < d) $ do
--             addOrTSB tsb (F v1) (T v2)
--           when (abs (y1 - x2) < d) $ do
--             addOrTSB tsb (T v1) (F v2)
--           when (abs (y1 - y2) < d) $ do
--             addOrTSB tsb (T v1) (T v2)
-- @
--
-- = Typical problems
-- - [ALPC H - Two Sat](https://atcoder.jp/contests/practice2/tasks/practice2_h).
--   See also: https://drken1215.hatenablog.com/entry/2023/05/07/001800
module Data.Graph.TwoSat where

-- TODO: use Bit

import Control.Monad
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST
import Data.Bool (bool)
import Data.Buffer
import Data.Graph.Sparse
import Data.Ix
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import ToyLib.Debug (dbgAssert)

-- | Data with @True@ | @False@ tag.
data TF a = T !a | F !a

-- | Converts the value and boolean pair into `TF`.
asTF :: a -> Bool -> TF a
asTF x True = T x
asTF x False = F x

-- | 2-satisfiability problem with constraints being added.
data TwoSatBuilder s = TwoSatBuilder
  { -- | The number of variables.
    nVarsTSB :: !Int,
    -- | Implication graph edges.
    bufTSB :: !(Buffer s (Int, Int))
  }

-- | \(O(V+E)\) Creates a two-sat builder.
newTSB :: (PrimMonad m) => Int -> Int -> m (TwoSatBuilder (PrimState m))
newTSB !nVarsTSB !nMaxClauses = do
  !bufTSB <- newBuffer (2 * nMaxClauses)
  return TwoSatBuilder {..}

-- | \(O(1)\) Adds an or clause: \(x1 = b1 || x2 = b2\).
addOrTSB :: (PrimMonad m) => TwoSatBuilder (PrimState m) -> TF Int -> TF Int -> m ()
addOrTSB !ts (T !x1) (T !x2) = addOrTSB' ts x1 True x2 True
addOrTSB !ts (T !x1) (F !x2) = addOrTSB' ts x1 True x2 False
addOrTSB !ts (F !x1) (T !x2) = addOrTSB' ts x1 False x2 True
addOrTSB !ts (F !x1) (F !x2) = addOrTSB' ts x1 False x2 False

-- | \(O(1)\) Adds an or clause: \(x1 = b1 || x2 = b2\).
addOrTSB' :: (PrimMonad m) => TwoSatBuilder (PrimState m) -> Int -> Bool -> Int -> Bool -> m ()
addOrTSB' TwoSatBuilder {..} x1 b1 x2 b2 = case (b1, b2) of
  -- x1 || x2 \iff not x1 => x2, not x2 => x1
  (True, True) -> do
    pushBack bufTSB (x1', x2)
    pushBack bufTSB (x2', x1)
  -- x1 || not x2 \iff not x1 => not x2, x2 => x1
  (True, False) -> do
    pushBack bufTSB (x1', x2')
    pushBack bufTSB (x2, x1)
  -- not x1 || x2 \iff x1 => x2, not x2 => not x1
  (False, True) -> do
    pushBack bufTSB (x1, x2)
    pushBack bufTSB (x2', x1')
  -- not x1 || not x2 \iff x1 => not x2, x2 => not x1
  (False, False) -> do
    pushBack bufTSB (x1, x2')
    pushBack bufTSB (x2, x1')
  where
    !_ = dbgAssert (inRange (0, nVarsTSB - 1) x1) $ "invalid var index: " ++ show x1
    !_ = dbgAssert (inRange (0, nVarsTSB - 1) x2) $ "invalid var index: " ++ show x2
    !x1' = x1 + nVarsTSB
    !x2' = x2 + nVarsTSB

-- | \(O(V + E)\)
solveTS :: Int -> U.Vector (Int, Int) -> Maybe (U.Vector Bool)
solveTS !nVars !constraints = do
  -- TODO: better SCC output
  let gr = buildSG (2 * nVars) constraints
  let !sccs = downSccSG gr
  let !groups = U.create $ do
        !vec <- UM.replicate (2 * nVars) (-1 :: Int)
        forM_ (zip [0 :: Int ..] sccs) $ \(!iScc, !scc) -> do
          forM_ scc $ \v -> do
            GM.write vec v iScc
        return vec

  let !saturatable = U.all (\x -> groups G.! x /= groups G.! (x + nVars)) (U.generate nVars id)
  if not saturatable
    then Nothing
    else Just $ U.map (== 1) $ U.create $ do
      -- FIXME: better uninit representation?
      !vec <- UM.replicate nVars (-1 :: Int)
      forM_ sccs $ \scc -> do
        forM_ scc $ \v -> do
          !prev <- UM.read vec (v `mod` nVars)
          when (prev == -1) $ do
            -- NOTE: We're seeing from the downstream vertices!!!!
            GM.write vec (v `mod` nVars) $ bool 1 0 (v < nVars)
      return vec

-- | \(O(V+E)\) The main interface of two-sat solve.
twoSat :: Int -> Int -> (forall s. TwoSatBuilder s -> ST s ()) -> Maybe (U.Vector Bool)
twoSat !nVars !nEdges f = runST $ do
  !tsb <- newTSB nVars nEdges
  f tsb
  !constraints <- unsafeFreezeBuffer (bufTSB tsb)
  return $ solveTS nVars constraints
