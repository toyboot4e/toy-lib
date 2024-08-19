{-# LANGUAGE CPP #-}
#include "./__import"
-- {{{ toy-lib import

import Data.Core.SemigroupAction
import Data.Core.SegmentTreeAction
import Data.Instances.Affine2d
import Data.ModInt
import Data.SegmentTree.Lazy
import Math.Stimes
import ToyLib.Parser
import ToyLib.Parser.Grid
import ToyLib.Prelude
import ToyLib.ShowBSB

-- }}} toy-lib import

debug :: Bool
debug = False

-- TODO: manage subtree size within the semgnet tree:
-- https://maspypy.com/library-checker-range-set-range-composite

{- ORMOLU_DISABLE -}
type MyModulo = (998244353 :: Nat) -- (1_000_000_007 :: Nat)
type MyModInt = ModInt MyModulo ; myMod :: Int ; myMod = fromInteger $ natVal' @MyModulo proxy# ; {-# INLINE modInt #-} ; modInt :: Int -> MyModInt ; modInt = ModInt . (`rem` myMod)
{- ORMOLU_ENABLE -}

-- | Add
type OpRepr = Affine2d MyModInt

instance Semigroup Op where
  {-# INLINE (<>) #-}
  new <> _old = new

instance Monoid Op where
  -- REMARK: be sure to implement identity operator
  {-# INLINE mempty #-}
  mempty = Op (Affine2d (ModInt (-1), ModInt (-1)))

instance SemigroupAction Op Acc where
  {-# INLINE sact #-}
  sact = segAct

instance SegmentTreeAction Op Acc where
  {-# INLINE segActWithLength #-}
  segActWithLength op@(Op f) x len
    | op == mempty = x
    | len == 1 = Dual f
    | otherwise = Dual $ stimes' len f

type Acc = Dual (Affine2d MyModInt)

{- ORMOLU_DISABLE -}
newtype Op = Op OpRepr deriving newtype (Eq, Ord, Show) ; unOp :: Op -> OpRepr ; unOp (Op x) = x; newtype instance U.MVector s Op = MV_Op (U.MVector s OpRepr) ; newtype instance U.Vector Op = V_Op (U.Vector OpRepr) ; deriving instance GM.MVector UM.MVector Op ; deriving instance G.Vector U.Vector Op ; instance U.Unbox Op ;
{- ORMOLU_ENABLE -}

solve :: StateT BS.ByteString IO ()
solve = do
  (!n, !q) <- ints2'
  abs <- U.replicateM n ints2'
  qs <- U.replicateM q $ do
    int' >>= \case
      0 -> (0 :: Word8,,,,) <$> int' <*> int1' <*> int' <*> int'
      1 -> (1 :: Word8,,,,-1) <$> int' <*> int1' <*> int'
      _ -> error "unreachable"

  -- Be sure to use `Dual`, as we want to foldr [f_l, ..., f_r].
  stree <- buildLSTree $ U.map (\(!a, !b) -> Dual (Affine2d (modInt a, modInt b))) abs

  res <- (`U.mapMaybeM` qs) $ \case
    (0, !l, !r, !c, !d) -> do
      sactLSTree stree l r $ Op (Affine2d (modInt c, modInt d))
      return Nothing
    (1, !l, !r, !x, -1) -> do
      Dual f <- foldLSTree stree l r
      return . Just . unV2 $ f `sact` toV2 (modInt x)
    _ -> error "unreachable"

  printBSB $ unlinesBSB res

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/range_set_range_composite
-- #lazy-segment-tree
main :: IO ()
main = runIO solve

