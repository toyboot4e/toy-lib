{-# LANGUAGE CPP #-}
#include "./__import"

-- {{{ toy-lib import
import Data.Buffer
import Data.Graph.Sparse
import Data.Graph.Tree.Lca
import Data.Graph.Tree.TreeSG
import Data.SafeList
import Data.UnionFind.Mutable
import Data.Vector.IxVector
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
  (!n, !m) <- ints2'
  !es <- U.replicateM m ints11'
  q <- int'
  qs <- U.replicateM q ints11'

  let !gr = buildSG n $ swapDupeU es

  let (!tree, !restVerts) = runST $ do
        uf <- newMUF n
        rest <- newBufferAsQueue (m - (n - 1))

        es' <- (`U.filterM` es) $ \(!v1, !v2) -> do
          b <- unifyMUF uf v1 v2
          unless b $ do
            pushBack rest v1
          return b

        let tree = buildSG n $ swapDupeU es'
        (tree,) <$> unsafeFreezeBuffer rest

  let !bfs = V.map (bfsSG gr) $ U.convert restVerts
  let !lcaCache = lcaCacheSG tree 0

  let fixed x
        | x < 0 = maxBound
        | otherwise = x

  let !res = (`U.map` qs) $ \(!v1, !v2) ->
        let !d1 = lcaLen lcaCache v1 v2
            !d2 = minimumOr maxBound $ (`V.map` bfs) $ \b -> fixed $ b U.! v1 + b U.! v2
         in min d1 d2

  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://atcoder.jp/contests/past202104-open/tasks/past202104_o
-- #LCA (lcaLen)
main :: IO ()
main = runIO solve
