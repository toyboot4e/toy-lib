{-# LANGUAGE CPP #-}
#include "./__import"
-- {{{ toy-lib import
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB
import Data.Graph.TwoSat
-- }}} toy-lib import

{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug :: Bool
debug = False

solve :: StateT BS.ByteString IO ()
solve = do
  (!_, !_, !n, !m) <- (,,,) <$> word' <*> word' <*> int' <*> int'
  qs <- U.replicateM m ints3'

  let !res = twoSat (n + 1) m $ \tsb -> do
        U.forM_ qs $ \(!u, !v, !_) -> do
          let !u' = if u > 0 then T u else F (-u)
          let !v' = if v > 0 then T v else F (-v)
          addOrTSB tsb u' v'

  case res of
    Just !bs -> do
      printBSB "s SATISFIABLE"
      putBSB $ showBSB "v "
      -- the 0th vertex is dummy
      putBSB . unwordsBSB . U.tail $ U.imap (\i b -> if b then i else -i) bs
      putBSB $ showBSB " 0"
    Nothing -> do
      printBSB "s UNSATISFIABLE"

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/two_sat
-- #two-sat
main :: IO ()
main = runIO solve
