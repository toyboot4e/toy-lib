{-# LANGUAGE CPP #-}
#include "./__import"

-- {{{ toy-lib import

import Data.IntervalSet
import Data.MultiSet
import Data.Vector.Extra
import ToyLib.Compat
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB

-- }}} toy-lib import

{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

-- }}}

debug = False

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !q) <- ints2'
  xs <- intsU'
  ixs <- U.replicateM q $ (,) <$> int1' <*> int'

  vec <- U.thaw xs
  let !ms0 = fromListMS $ U.toList xs
  (`evalStateT` ms0) $ do
    let !rm0 = U.foldl' (\rm x -> insertIS x x rm) emptyIS xs
    (`evalStateT` rm0) $ do
      U.forM_ ixs $ \(!i, !x') -> do
        !x <- UM.exchange vec i x'
        -- state $ \rm -> let !_ = traceShow rm () in ((), rm)

        -- delete x
        lift $ modify' $ decMS x
        unlessM (lift $ gets (memberMS x)) $ do
          modify' $ deleteIS x x

        -- insert x'
        lift $ modify' $ incMS x'
        whenM (lift $ gets ((== 1) . getMS x')) $ do
          modify' $ insertIS x' x'

        printBSB =<< gets mexIS

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc330/tasks/abc330_e
-- #range-map
main :: IO ()
main = runIO solve
