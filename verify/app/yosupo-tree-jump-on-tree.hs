{-# LANGUAGE CPP #-}
#include "./__import"

import Debug.Trace

-- {{{ toy-lib import

import Data.Graph.Sparse
import Data.Graph.Tree.Hld
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB

-- }}} toy-lib import
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug :: Bool
debug = False

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !q) <- ints2'
  es <- U.replicateM (n - 1) ints2'
  qs <- U.replicateM q ints3'
  let !gr = buildSG n $ swapDupeU es
  let !hld = hldOf gr
  -- let !_ = traceShow ("depth", depthHLD hld) ()
  let !res = (`U.map` qs) $ \(!u, !v, !i) ->
        -- let !_ = traceShow ("query", u, v, i) ()
         fromMaybe (-1) $ jumpHLD hld u v i
  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/jump_on_tree
-- #hld
main :: IO ()
main = runIO solve
