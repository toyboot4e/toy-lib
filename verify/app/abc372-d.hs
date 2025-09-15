{-# LANGUAGE CPP #-}
#include "./__import"

-- {{{ toy-lib import

import Algorithm.SlideMin
import Data.SegmentTree.Strict
import ToyLib.Contest.Prelude

-- }}} toy-lib import
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}
#ifdef DEBUG
debug :: Bool ; debug = True
#else
debug :: Bool ; debug = False
#endif
{- ORMOLU_ENABLE -}

solve :: StateT BS.ByteString IO ()
solve = do
  !n <- int'
  !hs <- intsU'

  let !imos = U.create $ do
        vec <- UM.replicate n (0 :: Int)
        let !ls = lookBackHigherIndices hs
        U.iforM_ ls $ \r l_ -> do
          let !l = max 0 l_
          GM.modify vec (+ 1) l
          GM.modify vec (subtract 1) r
        return vec

  printVec $ U.scanl1' (+) imos

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc372/tasks/abc372_d
main :: IO ()
main = runIO solve
