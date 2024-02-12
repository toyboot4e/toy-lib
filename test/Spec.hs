module Main where

import Test.DocTest

-- Running `doctest`:
-- $ cabal repl --with-ghc=doctest --repl-options='-w -Wdefault'

-- Running `haddock`:
-- $ cabal haddock
-- $ cabal haddock --open

main :: IO ()
main = do
  doctest ["src"]

