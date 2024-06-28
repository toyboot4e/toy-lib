{-# LANGUAGE CPP #-}
#include "./__import"

import Debug.Trace
-- {{{ toy-lib import

import Data.Graph.Sparse
import Data.UnionFind.Sparse
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB

-- }}} toy-lib import
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug :: Bool
debug = True

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !q) <- ints2'

  -- type, from, u, v
  qs <- U.replicateM q ints4'
  let !gr = buildSG_ (q + 1) $ G.imap (\(succ -> !i) (!_, succ -> !from, !_, !_) -> (from, i)) qs

  res <- UM.replicate q (-2 :: Int)
  let run i uf = do
        let (!t, !_, !u, !v) = U.unsafeIndex qs (i - 1)
        case t of
          0 -> do
            U.forM_ (gr `adj` i) $ \i' -> do
              let !uf' = unifySUF u v uf
              run i' uf'
          1 -> do
            GM.write res (i - 1) . bool 0 1 $ sameSUF u v uf
          _ -> error "unreachable"

  U.forM_ (gr `adj` 0) $ \i ->
    run i newSUF

  printBSB . unlinesBSB . U.filter (/= -2) =<< U.unsafeFreeze res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/persistent_unionfind
-- #sparse-union-find
main :: IO ()
main = runIO solve
