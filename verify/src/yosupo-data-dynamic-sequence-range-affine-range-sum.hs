{-# LANGUAGE CPP #-}
#include "./__import"
-- {{{ toy-lib import

import Data.Instances.Affine2d
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

  seq <- newSS (n + q)
  root0 <- allocSeqSS seq $ U.map Sum xs

  -- TODO: easier state monad, like `stateT` function?
  let f :: (Int, Int, Int, Int, Int) -> StateT SplayIndex (StateT BS.ByteString IO) (Maybe Int)
      f q = do
        root <- get
        case q of
          (0, !i, !x, !_, !_) -> do
            -- insert
            root' <- insertSS seq root i $ Sum x
            put root'
            return Nothing
          (1, !i, !_, !_, !_) -> do
            -- delete
            !root' <- deleteSS seq root i
            put root'
            return Nothing
          (2, !l, pred -> !r, !_, !_) -> do
            -- reverse
            root' <- reverseSS seq root l r
            put root'
            return Nothing
          (3, !l, pred -> !r, !b, !c) -> do
            -- apply affine transformation
            root' <- sactSS seq root l r $ Affine2d (b, c)
            put root'
            return Nothing
          (4, !l, pred -> !r, !_, !_) -> do
            -- fold
            (Sum !x, !root') <- foldSS seq root l r
            put root'
            return $ Just x

  (res :: U.Vector Int) <- (`evalStateT` root0) . (`U.mapMaybeM` qs) $ f

  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/dynamic_sequence_range_affine_range_sum
-- #splay-tree
main :: IO ()
main = runIO solve
