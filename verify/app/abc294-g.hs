{-# LANGUAGE CPP #-}
#include "./__import"

-- {{{ toy-lib import

import Data.Graph.Sparse
import Data.Graph.Tree.Hld
import ToyLib.Debug
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB

-- }}} toy-lib import
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug = False

-- }}}

solve :: StateT BS.ByteString IO ()
solve = do
  n <- int'
  es <- U.replicateM (n - 1) ints110'
  q <- int'
  qs <- U.replicateM q ints3'

  when (n == 1) $ do
    let cnt = U.length $ U.filter ((== 2) . fst3) qs
    printBSB $ unlinesBSB $ U.replicate cnt (0 :: Int)
    liftIO exitSuccess

  let !gr = buildWSG n $ swapDupeW es
  let !hld = hldOf gr

  -- REMARK: put edge weights to the deeper vertices
  let !ixs = edgeVertsHLD hld $ U.map (\(!u, !v, !w) -> (u, v, Sum w)) es
  !tm <- buildEdgeTM hld True ixs

  res <- (`U.mapMaybeM` qs) $ \case
    (1, pred -> !iEdge, !w') -> do
      -- let !_ = dbg (iEdge, es U.! iEdge, ixs U.! iEdge)
      writeTM tm (fst (ixs U.! iEdge)) (Sum w')
      return Nothing
    (2, pred -> !v1, pred -> !v2) -> do
      Just . getSum <$> foldTM tm v1 v2
    _ -> error "unreachable"

  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc294/tasks/abc294_g
-- #HLD (foldCommuteHLD)
main :: IO ()
main = runIO solve
