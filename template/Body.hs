main :: IO ()
main = runIO $ do
  !n <- int'
  !xs <- intsU'

  liftIO $ putStrLn "TODO"

  return ()
