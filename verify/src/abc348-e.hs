{-# LANGUAGE CPP #-}
#include "./__import"

-- {{{ toy-lib import

import Data.Core.SemigroupAction
import Data.Graph.Alias
import Data.Graph.Sparse
import Data.Graph.Tree.TreeSG
import ToyLib.Debug
import ToyLib.Parser
import ToyLib.Prelude
import ToyLib.ShowBSB

-- }}} toy-lib import

{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}

debug :: Bool
debug = False

-- }}}

-- | (acc, c)
type OpRepr = (Int, Int)

instance Semigroup Op where
  {-# INLINE (<>) #-}
  (Op (!a1, !c1)) <> (Op (!a2, !c2)) = Op (a1 + a2, c1 + c2)

instance Monoid Op where
  {-# INLINE mempty #-}
  mempty = Op (0, 0)

instance SemigroupAction Op Acc where
  {-# INLINE sact #-}
  sact (Op (!a, !c)) (Acc (!a0, !c0)) = Acc (a + c + a0, c + c0)

-- | (acc, c)
type AccRepr = (Int, Int)

{- ORMOLU_DISABLE -}
newtype Acc = Acc AccRepr deriving newtype (Eq, Ord, Show) ; unAcc :: Acc -> AccRepr ; unAcc (Acc x) = x ; newtype instance U.MVector s Acc = MV_Acc (U.MVector s AccRepr) ; newtype instance U.Vector Acc = V_Acc (U.Vector AccRepr) ; deriving instance GM.MVector UM.MVector Acc ; deriving instance G.Vector U.Vector Acc ; instance U.Unbox Acc ;
newtype Op = Op OpRepr deriving newtype (Eq, Ord, Show) ; newtype instance U.MVector s Op = MV_Op (U.MVector s OpRepr) ; newtype instance U.Vector Op = V_Op (U.Vector OpRepr) ; deriving instance GM.MVector UM.MVector Op ; deriving instance G.Vector U.Vector Op ; instance U.Unbox Op ;
{- ORMOLU_ENABLE -}

solve :: StateT BS.ByteString IO ()
solve = do
  !n <- int'
  !es <- U.replicateM (n - 1) ints11'
  !cs <- intsN' n

  let !gr = buildSG n $ swapDupeU es
  let !_ = dbg (scanTreeSG @U.Vector gr (0 :: Int) const sact (\v -> Acc (0, cs U.! v)) (Op . unAcc))

  let !res = dbgId $ foldTreeAllSG gr (\v -> Acc (0, cs U.! v)) (Op . unAcc)
  printBSB $ U.minimum $ U.map (fst . unAcc) res

-- verification-helper: PROBLEM https://atcoder.jp/contests/abc348/tasks/abc348_e
-- #rerooting
main :: IO ()
main = runIO solve
