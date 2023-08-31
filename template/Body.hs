data MyModulo = MyModulo

instance TypeInt MyModulo where
  -- typeInt _ = 1_000_000_007
  typeInt _ = 998244353

type MyModInt = ModInt MyModulo

myModulo :: Int
myModulo = typeInt (Proxy @MyModulo)

modInt :: Int -> MyModInt
modInt = ModInt . (`rem` myModulo)

undef :: Int
undef = -1

main :: IO ()
main = do
  !n <- ints1
  !xs <- intsVU

  putStrLn "TODO"
