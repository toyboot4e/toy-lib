{-# LANGUAGE CPP #-}
#include "./__import"

-- {{{ toy-lib import

import Data.ModInt
import Data.Graph.TwoSat
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB

-- }}} toy-lib import
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

-- }}}

debug :: Bool
debug = False

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !d) <- ints2'
  !xys <- U.replicateM n ints2'

  let !res = twoSat n (2 * n * pred n) $ \tsb -> do
        forM_ [0 .. n - 1] $ \v1 -> do
          let (!x1, !y1) = xys U.! v1
          forM_ [v1 + 1 .. n - 1] $ \v2 -> do
            let (!x2, !y2) = xys U.! v2
            when (abs (x1 - x2) < d) $ do
              addOrTSB tsb (F v1) (F v2)
            when (abs (x1 - y2) < d) $ do
              addOrTSB tsb (F v1) (T v2)
            when (abs (y1 - x2) < d) $ do
              addOrTSB tsb (T v1) (F v2)
            when (abs (y1 - y2) < d) $ do
              addOrTSB tsb (T v1) (T v2)

  case res of
    Nothing -> printBSB "No"
    Just !assignments -> do
      printBSB "Yes"
      let res = U.zipWith (\(!x, !y) b -> bool y x b) xys assignments
      printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://atcoder.jp/contests/practice2/tasks/practice2_h
-- #two-sat
--
-- REMARK: The test fails but the answer is correct.
-- TODO: Allow other solutions to the example out files
main :: IO ()
main = runIO solve
