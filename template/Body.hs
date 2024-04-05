solve :: StateT BS.ByteString IO ()
solve = do
  !n <- int'
  !xs <- intsU'

  liftIO $ putStrLn "TODO"

  return ()

main :: IO ()
main = runIO solve
