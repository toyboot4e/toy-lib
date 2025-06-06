{-# LANGUAGE CPP #-}
#include "./__import"
-- {{{ toy-lib import

import Data.Instances.Affine1
import Data.ModInt
import Data.SplaySeq
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB

-- }}} toy-lib import

{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug :: Bool
debug = False

{- ORMOLU_DISABLE -}
type MyModulo = (998244353 :: Nat) -- (1_000_000_007 :: Nat)
type MyModInt = ModInt MyModulo ; myMod :: Int ; myMod = fromInteger $ natVal' @MyModulo proxy# ; {-# INLINE modInt #-} ; modInt :: Int -> MyModInt ; modInt = ModInt . (`rem` myMod) ;
{- ORMOLU_ENABLE -}

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !q) <- ints2'
  xs <- intsU'
  qs <- U.replicateM q $ do
    int' >>= \case
      0 -> (0 :: Int,,,-1,-1) <$> int' <*> int'
      1 -> (1 :: Int,,-1,-1,-1) <$> int'
      2 -> (2 :: Int,,,-1,-1) <$> int' <*> int'
      3 -> (3 :: Int,,,,) <$> int' <*> int' <*> int' <*> int'
      4 -> (4 :: Int,,,-1,-1) <$> int' <*> int'
      _ -> error "unreachable"

  seq <- newLazySS (n + q)
  allocSeqSS seq $ U.map (Sum . ModInt @MyModulo) xs

  res <- (`U.mapMaybeM` qs) $ \case
    (0, !i, !x, !_, !_) -> do
      -- insert
      insertSS seq i $ Sum (ModInt x)
      return Nothing
    (1, !i, !_, !_, !_) -> do
      -- delete
      deleteSS seq i
      return Nothing
    (2, !l, pred -> !r, !_, !_) -> do
      -- reverse
      reverseSS seq l r
      return Nothing
    (3, !l, pred -> !r, !b, !c) -> do
      -- apply affine transformation
      sactSS seq l r $ Affine1 (ModInt @MyModulo b, ModInt @MyModulo c)
      return Nothing
    (4, !l, pred -> !r, !_, !_) -> do
      -- fold
      Sum (ModInt !x) <- foldSS seq l r
      return $ Just x

  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/dynamic_sequence_range_affine_range_sum
-- #splay-tree
main :: IO ()
main = runIO solve
