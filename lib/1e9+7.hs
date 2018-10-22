import Data.List

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
-- 低速
factorization :: Integer -> [Integer]
factorization 1 = []
factorization x = let v = head $ naiveFactors x
                  in  v : factorization (x `div` v)
-- 高速
primeFactors :: Integer -> [Integer]
primeFactors n | n < 2 = []
primeFactors n = go n [2,3,5]
   where
     go !n pps@(p:ps)
       | n < p*p   = [n]
       | r > 0     = go n ps
       | otherwise = p:go q pps
      where
        (q,r) = quotRem n p
     go n [] = [n]

-- 約数の列挙
-- simple but slow
naiveFactors :: Integer -> [Integer]
naiveFactors n = [x | x <- [2..n], n `mod` x == 0]

-- effective version
factors :: (Integral a) => a -> [a]
factors = Map.foldrWithKey (\k v acc -> products (powers k v) acc) [1] . toCountMap . primeFactors

powers :: (Integral a) => a -> a -> [a]
powers n m = [ n^i | i <- [0..m]]

products :: (Num a) => [a] -> [a] -> [a]
products xs ys = [ x*y | x <- xs, y <- ys]

toCountMap :: (Ord k, Integral a) => [k] -> Map.Map k a
toCountMap xs = Map.fromListWith (+) $ zip xs (repeat 1)



-- リストの最大公約数
listgcd :: (Integral a) => [a] -> a
listgcd [] = 1
listgcd [x] = x
listgcd (x:xs) = gcd x (listgcd xs)
