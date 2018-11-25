{-# LANGUAGE BangPatterns #-}
-- DISCO C - チップ・ストーリー　～白銀編～
import Data.List

main :: IO ()
main = do
  n <- readLn
  print $ solve n

solve n = foldr modadd 0 $ map f [1..n]
  where
    f c = ((power c 10) - (power (c-1) 10)) `modmul` (power (n `div` c) 10)


divConst = 10^9 + 7 :: Integer
modadd x y = (x + y) `mod` divConst
modsub x y = (x - y) `mod` divConst
modmul x y = ((x `mod` divConst) * (y `mod` divConst)) `mod` divConst
moddiv x y = modmul x (power y (divConst - 2))
power x y
  | y == 0 = 1
  | y == 1 = x `mod` divConst
  | y `mod` 2 == 0 = (power x (y `div` 2))^2 `mod` divConst
  | otherwise = (power x (y `div` 2))^2 * x `mod` divConst
