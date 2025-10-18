{-# LANGUAGE CPP #-}
#include "./__import"

-- {{{ toy-lib import

import Data.Graph.Generic
import Data.Graph.Sparse
import Data.UnionFind.Sparse
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

  -- type, from, u, v
  qs <- U.replicateM q ints4'
  let !gr = buildSG_ (q + 1) $ G.imap (\i0 (!_, !from0, !_, !_) -> (from0 + 1, i0 + 1)) qs

  res <- UM.replicate q (-2 :: Int)
  runPersistentDfs (gr `adjW`) 0 emptySUF $ \ !uf _ q1' _ -> do
    let (!t, !_, !u, !v) = U.unsafeIndex qs (q1' - 1)
    case t of
      0 -> do
        return $ unifySUF u v uf
      1 -> do
        GM.unsafeWrite res (q1' - 1) . bool 0 1 $ sameSUF u v uf
        return uf
      _ -> error "unreachable"

  printBSB . unlinesBSB . U.filter (/= -2) =<< U.unsafeFreeze res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/persistent_unionfind
-- #sparse-union-find
main :: IO ()
main = runIO solve
