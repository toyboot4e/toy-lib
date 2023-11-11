-- | Alias to `(Int, [Int])`.
module Data.SizedList where

-- | `(length, digitsInDescendingOrder)`
type SizedList = (Int, [Int])

compareSL :: SizedList -> SizedList -> Ordering
compareSL (!len1, !xs1) (!len2, !xs2)
  | len1 > len2 = GT
  | len1 < len2 = LT
  | otherwise = inner xs1 xs2
  where
    inner [] [] = EQ
    inner (y1 : ys1) (y2 : ys2) = case compare y1 y2 of
      EQ -> inner ys1 ys2
      c -> c
    inner _ [] = error "unreachable: `compareSL`"
    inner [] _ = error "unreachable: `compareSL`"

maxSL :: SizedList -> SizedList -> SizedList
maxSL sl1 sl2 = case compareSL sl1 sl2 of
  GT -> sl1
  _ -> sl2

nullSL :: SizedList -> Bool
nullSL = null . snd

emptySL :: SizedList
emptySL = (0, [])

consSL :: SizedList -> Int -> SizedList
consSL (!len, !xs) !x = (len + 1, x : xs)
