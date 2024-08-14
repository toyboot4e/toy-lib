{-# LANGUAGE CPP #-}
#include "./__import"
-- {{{ toy-lib import

import Data.Graph.Generic
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
  q <- int'
  qs <- U.replicateM q $ do
    int' >>= \case
      -- Vertex 0 is treated as the entry point
      0 -> (0 :: Int,,) <$> (succ <$> int') <*> int'
      1 -> (1 :: Int,,-1 :: Int) <$> (succ <$> int')
      _ -> error "unreachable"

  let !gr = buildSG_ (q + 1) $ U.imap (\v (!_, !u1, !_) -> (u1, v + 1)) qs
  res <- UM.replicate q (-1 :: Int)

  runPersistentDfs (gr `adjW`) 0 Seq.empty $ \ !seq !_ !v1 () -> do
    case U.unsafeIndex qs (v1 - 1) of
      (0, !_, !x) -> do
        return $ seq Seq.:|> x
      (1, !_, !_) -> do
        -- record the result
        let (front Seq.:<| seq') = seq
        GM.unsafeWrite res (v1 - 1) front
        return seq'
      _ -> do
        error "unreachable"

  res' <- U.filter (/= -1) <$> U.unsafeFreeze res
  printBSB $ unlinesBSB res'

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/persistent_queue
-- #two-sat
main :: IO ()
main = runIO solve
