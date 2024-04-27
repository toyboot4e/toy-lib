module Tests.Bisect where

import Algorithm.Bisect
import Data.List qualified as L
import Data.Vector.Unboxed qualified as U
import Test.Tasty
import Test.Tasty.QuickCheck as QC

intPartition :: Int -> Int -> (Int -> Bool) -> U.Vector Int -> (Maybe Int, Maybe Int)
intPartition l r p xs = case (U.null ls, U.null rs) of
  (True, True) -> error "unreachable"
  (False, True) -> (Just l', Nothing)
  (True, False) -> (Nothing, Just r')
  _ -> (Just l', Just r')
  where
    xs' = U.take (r + 1 - l) . U.drop l $ xs
    (ls, rs) = U.partition p xs'
    l' = l + U.length ls - 1
    r' = l' + 1

intBisectProps :: TestTree
intBisectProps =
  testGroup
    "Int bisect properties"
    [ QC.testProperty "bisect" $ do
        -- O(N^2 logN) (N = 100)
        n <- QC.chooseInt (1, 100)
        p <- QC.chooseInt (-25, 25)
        xs <- QC.vectorOf n (QC.chooseInt (-20, 20))
        let xs' = U.fromList $ L.sort xs
        let lrs = [(l, r) | l <- [0 .. n - 1], r <- [l + 1 .. n - 1]]
        let isYes = (<= p) . (xs' U.!)
        return $
          all
            ( \(l, r) ->
                let (il, ir) = bisect l r isYes
                 in (il, ir) == intPartition l r (<= p) xs'
                    && (il, ir) == (bisectL l r isYes, bisectR l r isYes)
            )
            lrs
    ]

-- TODO: test double bisect

tests :: TestTree
tests = testGroup "Algorithm.Bisect" [intBisectProps]
