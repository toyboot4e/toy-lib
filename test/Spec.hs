module Main where

import Test.DocTest

-- Running `doctest`:
-- $ cabal repl --with-ghc=doctest --repl-options='-w -Wdefault'

-- Running `haddock`:
-- $ cabal haddock
-- $ cabal haddock --open

-- | `stack test` where we run `doctest`.
main :: IO ()
main = do
  putStrLn "TEST"

