-- | Digit DP based on automatons.
module Data.DigitDp where

import Control.Monad (foldM)
import Control.Monad.Extra (concatMapM, mapMaybeM)
import Control.Monad.Identity (runIdentity)
import qualified Data.ByteString.Char8 as BS
import Data.Char (digitToInt)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable (hashWithSalt))
import Data.Semigroup (Max (..), Sum (..))

class DigitDfa context s d where
  onNextDigit :: context -> s -> d -> Maybe s

newtype AndDfa a = AndDfa a
  deriving (Eq, Show)

instance
  (DigitDfa context1 s1 d, DigitDfa context2 s2 d) =>
  DigitDfa (AndDfa (context1, context2)) (s1, s2) d
  where
  onNextDigit (AndDfa (!context1, !context2)) (!s1, !s2) d = do
    s1' <- onNextDigit context1 s1 d
    s2' <- onNextDigit context2 s2 d
    pure (s1', s2')

-- | Less than or equal to target value
data LtEq = LessThan | Equal
  deriving (Eq, Show, Enum)

instance Hashable LtEq where
  hashWithSalt i s = hashWithSalt i (fromEnum s)

-- | Tracks numbers less than or equal to L.
newtype LtEqDfa = LtEqDfa Int
  deriving (Eq, Show)

instance DigitDfa LtEqDfa LtEq Int where
  onNextDigit (LtEqDfa d) Equal i =
    case compare i d of
      EQ -> Just Equal
      LT -> Just LessThan
      GT -> Nothing
  onNextDigit (LtEqDfa _) LessThan _ =
    Just LessThan

-- | Counts the number of non-zero numbers in each digit.
newtype CountNonZero = CountNonZero Int
  deriving (Eq, Show)

instance Hashable CountNonZero where
  hashWithSalt :: Int -> CountNonZero -> Int
  hashWithSalt i (CountNonZero k) = hashWithSalt i k

newtype CountNonZeroDfa = CountNonZeroDfa Int
  deriving (Eq, Show)

instance DigitDfa CountNonZeroDfa CountNonZero Int where
  onNextDigit _ s 0 = Just s
  onNextDigit (CountNonZeroDfa k) (CountNonZero n) _
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
  onNextDigit (KnownValueModKDfa {k, digitUnit}) (KnownValueModK acc) d =
    Just $ KnownValueModK ((acc + d * digitUnit) `mod` k)

{-# INLINE foldlMDigitDp #-}
foldlMDigitDp ::
  forall m dfa s v d.
  (Monad m, DigitDfa dfa s d, Hashable s, Semigroup v) =>
  -- | Next input values
  (Char -> [d]) ->
  -- | Update value
  (v -> d -> v) ->
  -- | DFA creater
  (Char -> d -> m dfa) ->
  -- | After digit
  (Char -> m ()) ->
  -- | Initial hashmap
  HM.HashMap s v ->
  -- | The folding string
  BS.ByteString ->
  -- | Folding result
  m (HM.HashMap s v)
foldlMDigitDp toNextInputs updateValue createDfa afterDigit hm0 bs = foldM step hm0 $ BS.unpack bs
  where
    step :: HM.HashMap s v -> Char -> m (HM.HashMap s v)
    step sofar c = do
      next <-
        fmap (HM.fromListWith (<>))
          . concatMapM
            ( \(!state, !v) ->
                mapMaybeM
                  ( \d -> do
                      dfa <- createDfa c d
                      pure $ do
                        !state' <- onNextDigit dfa state d
                        let !v' = updateValue v d
                        Just (state', v')
                  )
                  nextInputs
            )
          $ HM.toList sofar
      afterDigit c
      pure next
      where
        !nextInputs = toNextInputs c

-- https://atcoder.jp/contests/agc021/tasks/agc021_a
-- 1. select your DFA in `createDfa`
-- 2. set the semigroup value in `hm0` of `HashMap`
solveAgc23A :: BS.ByteString -> Int
solveAgc23A s =
  let hm = runIdentity $ foldlMDigitDp toNextInputs updateValue createDfa afterDigit hm0 s
      accept _dfa = const True
   in getMax . foldMap snd $ filter (accept . fst) $ HM.toList hm
  where
    toNextInputs = const [0 .. 9]
    updateValue v d = v + Max d
    createDfa c _ = do
      pure . LtEqDfa $ digitToInt c
    afterDigit _ = do
      pure ()
    hm0 = HM.singleton Equal (Max (0 :: Int))

-- https://atcoder.jp/contests/abc154/tasks/abc154_e
-- 1. select your DFA in `createDfa`
-- 2. set the semigroup value in `hm0` of `HashMap`
solveAbc154E :: BS.ByteString -> Int -> Int
solveAbc154E s k =
  let hm = runIdentity $ foldlMDigitDp toNextInputs updateValue createDfa afterDigit hm0 s
      accept (!_, CountNonZero i) = i == k
   in getSum . foldMap snd $ filter (accept . fst) $ HM.toList hm
  where
    toNextInputs _c = [0 :: Int .. 9]
    updateValue v _d = v
    createDfa c _ = do
      pure $ AndDfa (LtEqDfa (digitToInt c), CountNonZeroDfa k)
    afterDigit _c = do
      pure ()
    hm0 = HM.singleton (Equal, CountNonZero (0 :: Int)) (Sum (1 :: Int))
