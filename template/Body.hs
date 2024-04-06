solve :: StateT BS.ByteString IO ()
solve = do
  !n <- int'
  !xs <- intsU'

  printBSB "TODO"

main :: IO ()
main = runIO solve
