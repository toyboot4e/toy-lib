{-# LANGUAGE CPP #-}
#include "./__import"

-- {{{ toy-lib import

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
  es <- U.generateM m $ \i -> (\(!u, !v, !w) -> (u, v, (w, i))) <$> ints3'
  let !mst = collectMST m es
  printBSB $ U.sum $ U.map (fst . thd3) mst
  printVec $ U.map (snd . thd3) mst

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/minimum_spanning_tree
-- #mst
main :: IO ()
main = runIO solve
