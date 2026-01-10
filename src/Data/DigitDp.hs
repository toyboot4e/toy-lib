-- | Digit DP based on automatons.
module Data.DigitDp where

import qualified AtCoder.ModInt as MI
import Control.Monad (foldM)
import Control.Monad.Extra (concatMapM, mapMaybeM)
import Control.Monad.Identity (runIdentity)
import qualified Data.ByteString.Char8 as BS
import Data.Char (digitToInt)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable (hashWithSalt))
import Data.Semigroup (Max (..), Sum (..))

class DigitDfa context s a where
  transiteDfa :: context -> s -> a -> Maybe s
  -- acceptDigitDfa :: s -> Bool

newtype AndDfa a = AndDfa a
  deriving (Eq, Show)

instance
  (DigitDfa context1 s1 a, DigitDfa context2 s2 a) =>
  DigitDfa (AndDfa (context1, context2)) (s1, s2) a
  where
  transiteDfa (AndDfa (!context1, !context2)) (!s1, !s2) a = do
    s1' <- transiteDfa context1 s1 a
    s2' <- transiteDfa context2 s2 a
    pure (s1', s2')

-- | Less than or equal to target value
data LtEq = LessThan | Equal
  deriving (Eq, Show, Enum)

instance Semigroup LtEq where
  LessThan <> _ = LessThan
  Equal <> rhs = rhs

instance Hashable LtEq where
  hashWithSalt i s = hashWithSalt i (fromEnum s)

-- | Tracks numbers less than or equal to L.
newtype LtEqDfa = LtEqDfa Int
  deriving (Eq, Show)

instance DigitDfa LtEqDfa LtEq Int where
  transiteDfa (LtEqDfa a) Equal i =
    case compare i a of
      EQ -> Just Equal
      LT -> Just LessThan
      GT -> Nothing
  transiteDfa (LtEqDfa _) LessThan _ =
    Just LessThan

-- | LtEq from the front and Ordering from the back
data LtEqWithBackHalfOrdering = LtEqWithBackHalfOrdering LtEq Ordering
  deriving (Eq, Show)

instance Hashable LtEqWithBackHalfOrdering where
  hashWithSalt i (LtEqWithBackHalfOrdering ltEq ord) = hashWithSalt (hashWithSalt i ltEq) ord

-- | A pair of (front, back) characters.
newtype LtEqWithBackHalfOrderingDfa = LtEqWithBackHalfOrderingDfa (Int, Int)
  deriving (Eq, Show)

-- Note that the earlier part (*) takes higher precedence in `backHalfOrd`:
-- 1 2 3 0 1 <-- original array
--       *
-- 1 2 3 2 1 <-- an Equal palindrome
instance DigitDfa LtEqWithBackHalfOrderingDfa LtEqWithBackHalfOrdering Int where
  transiteDfa (LtEqWithBackHalfOrderingDfa (!f, !b)) (LtEqWithBackHalfOrdering Equal backHalfOrd) a =
    case compare a f of
      EQ -> Just $ LtEqWithBackHalfOrdering Equal $ compare a b <> backHalfOrd
      LT -> Just $ LtEqWithBackHalfOrdering LessThan $ compare a b <> backHalfOrd
      GT -> Nothing
  transiteDfa (LtEqWithBackHalfOrderingDfa (!_, !b)) (LtEqWithBackHalfOrdering LessThan backHalfOrd) a =
    Just $ LtEqWithBackHalfOrdering LessThan $ compare a b <> backHalfOrd

-- | Counts the number of non-zero numbers in each digit.
newtype CountNonZero = CountNonZero Int
  deriving (Eq, Show)

instance Hashable CountNonZero where
  hashWithSalt :: Int -> CountNonZero -> Int
  hashWithSalt i (CountNonZero k) = hashWithSalt i k

newtype CountNonZeroDfa = CountNonZeroDfa Int
  deriving (Eq, Show)

instance DigitDfa CountNonZeroDfa CountNonZero Int where
  transiteDfa _ s 0 = Just s
  transiteDfa (CountNonZeroDfa k) (CountNonZero n) _
    | n >= k = Nothing
    | otherwise = Just $ CountNonZero (n + 1)

-- | The known value at this point (in the context of digit DP) mod k.
newtype KnownValueModK = KnownValueModK Int
  deriving (Eq, Show)

instance Hashable KnownValueModK where
  hashWithSalt :: Int -> KnownValueModK -> Int
  hashWithSalt i (KnownValueModK k) = hashWithSalt i k

data KnownValueModKDfa = KnownValueModKDfa
  { k :: {-# UNPACK #-} !Int,
    digitUnit :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show)

instance DigitDfa KnownValueModKDfa KnownValueModK Int where
  transiteDfa (KnownValueModKDfa {k, digitUnit}) (KnownValueModK acc) d =
    Just $ KnownValueModK ((acc + d * digitUnit) `mod` k)

{-# INLINE foldlMDigitDp #-}
foldlMDigitDp ::
  forall m dfa s v a t x.
  (Monad m, DigitDfa dfa s a, Hashable s, Semigroup v, Foldable t) =>
  -- | Next input values (alphabets)
  (x -> [a]) ->
  -- | Update value
  (v -> a -> v) ->
  -- | DFA creater
  (x -> a -> m dfa) ->
  -- | After digit
  (x -> m ()) ->
  -- | Initial hashmap
  HM.HashMap s v ->
  -- | The folding string
  t x ->
  -- | Folding result
  m (HM.HashMap s v)
foldlMDigitDp toNextInputs updateValue createDfa afterDigit = foldM step
  where
    step :: HM.HashMap s v -> x -> m (HM.HashMap s v)
    step sofar x = do
      next <-
        fmap (HM.fromListWith (<>))
          . concatMapM
            ( \(!state, !v) ->
                mapMaybeM
                  ( \a -> do
                      dfa <- createDfa x a
                      pure $ do
                        !state' <- transiteDfa dfa state a
                        let !v' = updateValue v a
                        Just (state', v')
                  )
                  nextInputs
            )
          $ HM.toList sofar
      afterDigit x -- TODO: do we ever need it?
      pure next
      where
        !nextInputs = toNextInputs x

-- https://atcoder.jp/contests/agc021/tasks/agc021_a
-- 1. select your DFA in `createDfa`
-- 2. set the semigroup value in `hm0` of `HashMap`
solveAgc23A :: [Char] -> Int
solveAgc23A s =
  let hm = runIdentity $ foldlMDigitDp toNextInputs updateValue createDfa afterDigit hm0 s
      accept _dfa = True
   in getMax . foldMap snd $ filter (accept . fst) $ HM.toList hm
  where
    toNextInputs _x = [0 .. 9]
    updateValue v a = v + Max a
    createDfa x _a = do
      pure . LtEqDfa $ digitToInt x
    afterDigit _x = do
      pure ()
    hm0 = HM.singleton Equal (Max (0 :: Int))

-- https://atcoder.jp/contests/abc154/tasks/abc154_e
-- 1. select your DFA in `createDfa`
-- 2. set the semigroup value in `hm0` of `HashMap`
solveAbc154E :: [Char] -> Int -> Int
solveAbc154E s k =
  let hm = runIdentity $ foldlMDigitDp toNextInputs updateValue createDfa afterDigit hm0 s
      accept (!_, CountNonZero i) = i == k
   in getSum . foldMap snd $ filter (accept . fst) $ HM.toList hm
  where
    toNextInputs _x = [0 :: Int .. 9]
    updateValue v _d = v
    createDfa x _a = do
      pure $ AndDfa (LtEqDfa (digitToInt x), CountNonZeroDfa k)
    afterDigit _c = do
      pure ()
    hm0 = HM.singleton (Equal, CountNonZero (0 :: Int)) (Sum (1 :: Int))

-- target: string ([A-Z]*)
solve242E :: BS.ByteString -> MI.ModInt 998244353
solve242E s =
  let -- In order to create a palindrome: (TODO: create alphabets automatically)
      s' = take ((BS.length s + 1) `div` 2) $ BS.zipWith (,) s (BS.reverse s)
      hm = runIdentity $ foldlMDigitDp toNextInputs updateValue createDfa afterDigit hm0 s'
      accept (LtEqWithBackHalfOrdering Equal !backOrd) =
        let !_ = dbg backOrd in
        backOrd /= GT
      accept (LtEqWithBackHalfOrdering LessThan !_) = True
   in getSum . foldMap snd . filter (accept . fst) $ HM.toList hm
  where
    toNextInputs _x = [0 :: Int .. 25]
    updateValue v _a = v
    createDfa (!f, !b) _a = do
      pure $ LtEqWithBackHalfOrderingDfa (ord f - ord 'A', ord b - ord 'A')
    afterDigit _x = pure ()
    hm0 = HM.singleton (LtEqWithBackHalfOrdering Equal EQ) (Sum (modInt 1))
