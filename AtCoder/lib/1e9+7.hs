import Data.List

divConst = 10^9 + 7 :: Integer
modmul x y = ((x `mod` divConst) * (y `mod` divConst)) `mod` divConst
moddiv x y = modmul x (power y (divConst - 2))
power x y
  | y == 0 = 1
  | y == 1 = x `mod` divConst
  | y `mod` 2 == 0 = (power x (y `div` 2))^2 `mod` divConst
  | otherwise = (power x (y `div` 2))^2 * x `mod` divConst

-- 階乗 n!
factorial :: Integer -> Integer
factorial n = foldr modmul 1 [1..n]

-- n! / m!
fact :: Integer -> Integer -> Integer
fact n m = foldr modmul 1 [(m+1)..n]

-- 組み合わせ nHc = (n + c - 1)!/(n! * (c - 1)!)
-- n個のボールとc枚の仕切り板
combination :: Integer -> Integer -> Integer
combination n c = foldl' moddiv (fact (n + c - 1) (c - 1)) [1..n]

-- 素因数分解
-- *Main> factorization 123456
-- [2,2,2,2,2,2,3,643]
factorization :: Integer -> [Integer]
factorization 1 = []
factorization x = let v = head $ factors x
                  in  v : factorization (x `div` v)

factors :: Integer -> [Integer]
factors n = [x | x <- [2..n], n `mod` x == 0]
