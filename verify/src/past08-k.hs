{-# LANGUAGE CPP #-}
#include "./__import"

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
