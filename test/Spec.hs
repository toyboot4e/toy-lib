import Test.DocTest

-- | `stack test` where we run `doctest`.
main :: IO ()
main = do
  -- Run `doctest` over all the source files:
  doctest ["-isrc", "src/"]
