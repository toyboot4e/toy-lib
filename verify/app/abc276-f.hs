{-# LANGUAGE CPP #-}
#include "./__import"

-- {{{ toy-lib import
import Algorithm.Bisect
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
  (!n, !m, !k) <- ints3'
  abs <- U.replicateM n ints2'
  cds <- U.replicateM m ints2'

  -- For such a maximixation (+counting) proble, write a formula and sort it out with respect to
  -- ther indices. Then it's made of constributions from each item:
  -- (a1 + a2) / ((a1 + b1) + (a2 + b2)) >= P
  -- (a1 - P (a1 + b1)) + (a2 - P (a2 + b2)) >= 0

  let !eps = 10.0 ** (-12.0) :: Double
  let !res = fromJust $ bisectRF64 eps 0.0 1.0 $ \p ->
        let abs' = U.map (\(!a, !b) -> intToDouble a - p * intToDouble (a + b)) abs
            !cds' = U.modify (VAI.sortBy (comparing Down)) $ U.map (\(!a, !b) -> intToDouble a - p * intToDouble (a + b)) cds
         in dbgId . (>= k) . note p . U.sum . (`U.map` abs') $ \x1 ->
              maybe 0 (+ 1) $ bisectL 0 (m - 1) (\i2 -> x1 + cds' U.! i2 >= 0.0)

  printBSB $ 100.0 * res

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc294/tasks/abc294_f
-- verification-helper: ERROR 0.000000001
-- #2分探索(小数)
main :: IO ()
main = runIO solve
