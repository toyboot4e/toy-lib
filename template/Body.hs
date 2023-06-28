data MyModulus = MyModulus

instance TypeInt MyModulus where
  -- typeInt _ = 1_000_000_007
  typeInt _ = 998244353

type MyModInt = ModInt MyModulus

modInt :: Int -> MyModInt
modInt = ModInt . (`mod` typeInt (Proxy @MyModulus))

undef :: Int
undef = -1
main :: IO ()
main = do
  [n] <- ints
  !xs <- intsVU

  putStrLn "TODO"
