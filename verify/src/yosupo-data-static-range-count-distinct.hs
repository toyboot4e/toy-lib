{-# LANGUAGE CPP #-}
#include "./__import"
-- {{{ toy-lib import

import Data.Vector.Extra
import Data.WaveletMatrix
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

  when (n == 0) $ do
    printBSB $ unlinesBSB (U.replicate q (0 :: Int))
    liftIO exitSuccess

  xs <- intsU'
  lrs <- U.replicateM q ints2'

  -- 補集合を考えると、重複した点の数を数えれば良い
  --
  -- 重複する点を数えるためには、以下のように近隣の重複点の位置を WM に入れて
  -- freq で取り出せば良い:
  --
  --    input      *  .  .  *  .  *  .  *
  --    i          0  1  2  3  4  5  6  7
  --    input'     -        0     3     5
  --                        <----->        [3, 5] 中の x \in [3, 5] の数は 1 (重複数は 1)
  --                        <----------->  [3, 7] 中の x \in [3, 7] の数は 2 (重複数は 1)
  --
  -- というのを人の提出から学んだ。どうやれば思いつくんだ……
  let xs' = U.create $ do
        vec <- UM.replicate n (-1 :: Int)
        let !dict = U.uniq $ U.modify VAI.sort xs
        lastIndex <- UM.replicate (G.length dict) (-1 :: Int)
        U.iforM_ (U.map (bindex dict) xs) $ \i x -> do
          iLast <- GM.exchange lastIndex x i
          unless (iLast == -1) $ do
            GM.write vec i iLast
        return vec

  -- let !_ = traceShow (xs, xs') ()
  let !wm = buildWM xs'

  -- 補集合のアイデア:
  -- let res = U.map (\(!l, !r) -> (r - l) - freqInWM wm l (r - 1) l (r - 1)) lrs

  -- 直接数える:
  let res = U.map (\(!l, !r) -> freqInWM wm l (r - 1) (-1) (l - 1)) lrs

  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/static_range_count_distinct
-- #wavelet-matrix
main :: IO ()
main = runIO solve
