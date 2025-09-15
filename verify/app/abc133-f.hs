{-# LANGUAGE CPP #-}
#include "./__import"

-- {{{ toy-lib import
import Algorithm.Bisect
import ToyLib.Debug
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB
import Data.Graph.Sparse
import Data.Graph.Tree.Hld

-- }}} toy-lib import
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug = False

-- Thanks: https://atcoder.jp/contests/abc133/submissions/34716233
solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !q) <- ints2'
  es <- U.replicateM (n - 1) ((\a b c d -> (a, b, (c, d))) <$> int1' <*> int1' <*> int' <*> int')
  !qs <- U.replicateM q ((,,,) <$> int' <*> int' <*> int1' <*> int1')

  let !gr = buildWSG n $ swapDupeW es

  -- HLD for LCA only
  let !hld = dbgId $ hldOf gr

  -- Run DFS just once and answer all the questions.
  --     * <-- root
  --     * <-- lca
  --    * *
  --    * * <-- v2
  --    * <-- v1
  -- query(c, d', v1, v2) := dist(root, v1) + dist(root, v2) - 2 * dist(root, lca(v1, v2))
  --   where
  --     dist(v1, v2) := rawDist(v1, v2) - colorEdgeDist(v1, v2, c) + d' * numVerts(v1, v2, c)

  let !_ = dbg ("build queries")
  let !query = runST $ do
        vec <- VM.replicate n []
        U.iforM_ qs $ \iq (!c, !d, !v1, !v2) -> do
          -- (i, c, len, weight)
          VM.modify vec ((iq, c, d, 1 :: Int) :) v1
          VM.modify vec ((iq, c, d, 1) :) v2
          VM.modify vec ((iq, c, d, -2) :) $ lcaHLD hld v1 v2
        V.map U.fromList <$> V.unsafeFreeze vec

  ans <- UM.replicate q 0

  -- color -> dist sum
  tot <- UM.replicate n (0 :: Int)
  -- color -> num verts
  cnt <- UM.replicate n (0 :: Int)

  fix3 0 (-1) 0 $ \loop cur prev dist -> do
    U.forM_ (query V.! cur) $ \(!i, !c, !len, !weight) -> do
      t <- UM.read tot c
      c <- UM.read cnt c
      UM.modify ans (+ ((dist - t + c * len) * weight)) i

    U.forM_ (gr `adjW` cur) $ \(!next, (!c, !len)) -> do
      when (next /= prev) $ do
        UM.modify tot (+ len) c
        UM.modify cnt (+ 1) c
        loop next cur (dist + len)
        UM.modify tot (subtract len) c
        UM.modify cnt (subtract 1) c

  !res <- U.unsafeFreeze ans
  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc133/tasks/abc133_f
-- #HLD (lcaHLD)
main :: IO ()
main = runIO solve
