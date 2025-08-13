{-# LANGUAGE LambdaCase #-}

import Data.Vector.Unboxed qualified as U

-- {{{ toy-lib import
import Data.Buffer
import ToyLib.Parser
import ToyLib.ShowBSB
-- }}} toy-lib import

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/deque
main :: IO ()
main = runIO $ do
  q <- int'
  qs <-
    U.replicateM q $ do
        int' >>= \case
          2 -> pure (2, -1)
          3 -> pure (3, -1)
          a -> (a,) <$> int'

  deq <- newBufferAsDeque q
  res <- (`U.mapMaybeM` qs) $ \case
    (0, !x) -> do
      pushFront deq x
      pure Nothing
    (1, !x) -> do
      pushBack deq x
      pure Nothing
    (2, !_) -> do
      popFront_ deq
      pure Nothing
    (3, !_) -> do
      popBack_ deq
      pure Nothing
    (4, !i) -> do
      Just <$> readFront deq i
    _ -> error "unreachable"

  printBSB $ unlinesBSB res
