solve :: StateT BS.ByteString IO ()
solve = do
  !n <- intP
  !xs <- intsP

  printBSB "TODO"

-- verification-helper: PROBLEM <<url>>
main :: IO ()
main = runIO solve
