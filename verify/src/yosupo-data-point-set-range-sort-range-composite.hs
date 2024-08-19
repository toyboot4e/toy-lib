{-# LANGUAGE CPP #-}
#include "./__import"
-- {{{ toy-lib import
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB
-- }}} toy-lib import

{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug :: Bool
debug = False

pattern NONE, SORTED_L, SORTED_R :: Int
pattern NONE = 0
pattern SORTED_L = 1
pattern SORTED_R = 2

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !q) <- ints2'
  pabs <- U.replicateM n ints3'
  qs <- U.replicateM q $ int' >>= \case
    0 -> (0 :: Int,,,,) <$> int' <*> int' <*> int' <*> int'
    1 -> (0 :: Int,,,, -1) <$> int' <*> int' <*> int'
    2 -> (0 :: Int,,,-1 :: Int, -1) <$> int' <*> int'
    3 -> (0 :: Int,,,-1 :: Int, -1) <$> int' <*> int'

  let !xs = U.map (\(!p, !a, !b) -> (p, Dual (Affine2d (a, b)))) pabs
  vec <- U.thaw xs

  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/point_set_range_sort_range_composite
-- #sqrt-decomposition
main :: IO ()
main = runIO solve

