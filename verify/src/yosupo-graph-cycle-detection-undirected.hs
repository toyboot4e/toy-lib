{-# LANGUAGE CPP #-}
#include "./__import"

-- {{{ toy-lib import

import Data.Buffer
import Data.Graph.Alias
import Data.Graph.Sparse
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB

-- }}} toy-lib import
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug :: Bool
debug = False

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !m) <- ints2'
  es <- U.generateM m $ \i -> (\(!u, !v) -> (u, v, i)) <$> ints2'

  let gr = buildWSG n $ swapDupeW es
  case findCycleUndirectedSG gr of
    Nothing -> printBSB "-1"
    Just uvws -> do
      printBSB $ U.length uvws
      printVec $ U.map fst3 uvws
      printVec $ U.map thd3 uvws

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/cycle_detection_undirected
-- #cycles
main :: IO ()
main = runIO solve
