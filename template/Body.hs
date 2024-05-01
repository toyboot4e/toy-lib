solve :: StateT BS.ByteString IO ()
solve = do
  !n <- int'
  !xs <- intsU'

  printBSB "TODO"

-- verification-helper: PROBLEM <<url>>
main :: IO ()
main = runIO solve
