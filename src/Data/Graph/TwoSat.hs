{-# LANGUAGE RecordWildCards #-}

-- | \(O(V + E)\) 2-satisfiability problem.
module Data.Graph.TwoSat where

import Control.Monad
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST
import Data.Bool (bool)
import Data.Buffer
import Data.Graph.Sparse
import Data.Ix
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import ToyLib.Debug (dbgAssert)

-- | Data with @True@ | @False@ tag.
data TF a = T !a | F !a

-- | Constructs `TF` dynamically.
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

newTSB :: (PrimMonad m) => Int -> Int -> m (TwoSatBuilder (PrimState m))
newTSB !nVarsTSB !nMaxEdges = do
  !bufTSB <- newBufferAsQueue nMaxEdges
  return TwoSatBuilder {..}

-- | Adds an or clause of $x1 = b1 || x2 = b2$.
addOrClauseTSB :: (PrimMonad m) => TwoSatBuilder (PrimState m) -> TF Int -> TF Int -> m ()
addOrClauseTSB !ts (T !x1) (T !x2) = addOrClauseTSB' ts x1 True x2 True
addOrClauseTSB !ts (T !x1) (F !x2) = addOrClauseTSB' ts x1 True x2 False
addOrClauseTSB !ts (F !x1) (T !x2) = addOrClauseTSB' ts x1 False x2 True
addOrClauseTSB !ts (F !x1) (F !x2) = addOrClauseTSB' ts x1 False x2 False

addOrClauseTSB' :: (PrimMonad m) => TwoSatBuilder (PrimState m) -> Int -> Bool -> Int -> Bool -> m ()
addOrClauseTSB' TwoSatBuilder {..} x1 b1 x2 b2 = case (b1, b2) of
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
  let !sccs = revTopSccSG gr
  let !groups = U.create $ do
        !vec <- UM.replicate (2 * nVars) (-1 :: Int)
        forM_ (zip [0 :: Int ..] sccs) $ \(!iScc, !scc) -> do
          forM_ scc $ \v -> do
            UM.write vec v iScc
        return vec

  let !saturatable = U.all (\x -> groups U.! x /= groups U.! (x + nVars)) (U.generate nVars id)
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
            UM.write vec (v `mod` nVars) $ bool 1 0 (v < nVars)
      return vec

twoSat :: Int -> Int -> (forall s. TwoSatBuilder s -> ST s ()) -> Maybe (U.Vector Bool)
twoSat !nVars !nEdges f = runST $ do
  !tsb <- newTSB nVars nEdges
  f tsb
  !constraints <- unsafeFreezeBuffer (bufTSB tsb)
  return $ solveTS nVars constraints
