{-# LANGUAGE CPP #-}
#include "./__import"

-- {{{ toy-lib import
import Data.DenseIntervalMap
import Data.Vector.Extra
import ToyLib.Compat
import ToyLib.Debug
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB
-- }}} toy-lib import

{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug = False

solve :: StateT BS.ByteString IO ()
solve = do
  n <- int'
  xs <- intsU'
  q <- int'
  lrxs <- U.replicateM q ints110'

  let !dict = U.modify VAI.sort $ xs U.++ U.map thd3 lrxs
  res <- UM.replicate 1 (0 :: Int)
  cnt <- UM.replicate (G.length dict) (0 :: Int)

  let onAdd l r x = do
        let !i = bindex dict x
        let !len = r - l + 1
        UM.read cnt i >>= \n' -> UM.modify res (subtract (n' * pred n' `div` 2)) 0
        UM.modify cnt (+ len) i
        UM.read cnt i >>= \n' -> UM.modify res (+ (n' * pred n' `div` 2)) 0
  let onDel l r x = do
        let !i = bindex dict x
        let !len = r - l + 1
        UM.read cnt i >>= \n' -> UM.modify res (subtract (n' * pred n' `div` 2)) 0
        UM.modify cnt (subtract len) i
        UM.read cnt i >>= \n' -> UM.modify res (+ (n' * pred n' `div` 2)) 0

  dm <- fromVecMDM xs onAdd
  U.forM_ lrxs $ \(!l, !r, !x) -> do
    insertMDM dm l r x onAdd onDel
    printBSB =<< UM.read res 0

-- verification-helper: PROBLEM https://atcoder.jp/contests/past202104-open/tasks/past202104_m
-- #range-map
main :: IO ()
main = runIO solve
