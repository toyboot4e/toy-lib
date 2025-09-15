{-# LANGUAGE CPP #-}
#include "./__import"
-- {{{ toy-lib import

import Data.Graph.MaxFlow
import Data.Vector.IxVector
import ToyLib.Debug
import ToyLib.Parser
import ToyLib.Parser.Grid
import ToyLib.Prelude
import ToyLib.ShowBSB
import ToyLib.ShowBSB.Grid

-- }}} toy-lib import

{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug :: Bool
debug = False

paintGrid :: (PrimMonad m) => IxMUVector (PrimState m) (Int, Int) Char -> Int -> Int -> m ()
paintGrid gr v1 v2
  | v1 >= h * w || v2 >= h * w = return ()
  | otherwise = case (dy, dx) of
      (0, 1) -> do
        writeIV gr (y1, x1) '>'
        writeIV gr (y2, x2) '<'
        where
          !_ = dbg ((y1, x1), (y2, x2))
      (0, -1) -> do
        writeIV gr (y1, x1) '<'
        writeIV gr (y2, x2) '>'
        where
          !_ = dbg ((y1, x1), (y2, x2))
      (1, 0) -> do
        writeIV gr (y1, x1) 'v'
        writeIV gr (y2, x2) '^'
        where
          !_ = dbg ((y1, x1), (y2, x2))
      (-1, 0) -> do
        writeIV gr (y1, x1) '^'
        writeIV gr (y2, x2) 'v'
        where
          !_ = dbg ((y1, x1), (y2, x2))
      _ -> error "unreachable"
  where
    (!h, !w) = boundsSize2 (boundsIV gr)
    (!y1, !x1) = v1 `divMod` w
    (!y2, !x2) = v2 `divMod` w
    !dy = y2 - y1
    !dx = x2 - x1

boundsSize2 :: ((Int, Int), (Int, Int)) -> (Int, Int)
boundsSize2 ((!y1, !x1), (!y2, !x2)) = (y2 - y1 + 1, x2 - x1 + 1)

solve :: StateT BS.ByteString IO ()
solve = do
  (!h, !w) <- ints2'
  !gr <- getGrid' h w

  let !bnd = boundsIV gr

  let !iStart = h * w
  let !iEnd = iStart + 1

  let !w1 = 1 :: Int

  -- REMARK: Do not try to filter with `odd i` or `even i`. It fails when `w` is even.
  let !isBlack = even . uncurry (+) . (`divMod` w)

  let !starts = U.map (iStart,,w1) . U.filter isBlack $ U.generate (h * w) id
  let !ends = U.map (,iEnd,w1) . U.filter (not . isBlack) $ U.generate (h * w) id

  let !tweens = U.concatMap f . U.filter isBlack $ U.generate (h * w) id
        where
          f i
            | gr @! (y, x) == '#' = U.empty
            | otherwise = U.map toEdge yxs
            where
              (!y, !x) = i `divMod` w
              yxs = U.filter ((/= '#') . (gr @!)) $ ortho4' bnd (y, x)
              toEdge (!y', !x') = (i, i', 1 :: Int)
                where
                  i' = w * y' + x'
                  !_ = dbgAssert (i /= i' && (y, x) /= (y', x')) $ show (i, i', (y, x), (y', x'))

  let !edges = starts U.++ ends U.++ tweens
  (!flow, mf) <- maxFlow' (h * w + 2) iStart iEnd edges

  !gr' <- thawIV gr

  -- TODO: Easier forward edge iteration
  es' <- edgesMF mf
  U.forM_ es' $ \(!v1, !v2, !_cap, !flow) -> do
    when (isBlack v1 && v1 < (h * w) && v2 < (h * w)) $ do
      when (flow == 1) $ do
        paintGrid gr' v1 v2

  !res <- unsafeFreezeIV gr'
  printBSB flow
  printGrid res

-- verification-helper: PROBLEM https://atcoder.jp/contests/practice2/tasks/practice2_d
-- #max-flow
--
-- REMARK: The test fails but the answer is correct.
-- TODO: allow non-perfect match
main :: IO ()
main = runIO solve
