{-# LANGUAGE CPP #-}
#include "./__import"

-- {{{ toy-lib import

import Data.BinaryHeap
import Data.Buffer
import Data.Graph.Alias
import Data.Graph.Sparse
import ToyLib.Debug
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB

-- }}} toy-lib import
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug :: Bool
debug = False

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !m, !src, !sink) <- ints4'
  es <- U.replicateM m ints3'

  let gr = buildWSG n es
  let (!dists, !parents) = djTreeSG gr (-1) $ U.singleton src

  case dists U.! sink of
    (-1) -> printBSB "-1"
    d -> do
      let !pathVerts = restorePath parents sink
      printBSB (d, U.length pathVerts - 1)
      printBSB . unlinesBSB $ U.zip pathVerts (U.tail pathVerts)

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/shortest_path
-- #dijkstra
main :: IO ()
main = runIO solve
