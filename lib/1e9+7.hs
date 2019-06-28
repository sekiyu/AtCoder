{-# LANGUAGE BangPatterns #-}
import Data.List
import qualified Data.Map.Strict as Map


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


-- nCk = n! / (k! * (n - k)!)
-- n個からk個えらぶ
select :: Integer -> Integer -> Integer
select n k = foldl' moddiv (fact n (n - k)) [1..k]

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
primeFactors n = go n [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293,
  307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601,
  607, 613, 617, 619, 631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739, 743, 751, 757, 761, 769, 773, 787, 797, 809, 811, 821, 823, 827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997, 1009]
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
--factors :: (Integral a) => a -> [a]
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
