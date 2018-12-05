import Data.List


-- 階乗 n!
factorial :: Int -> Int
factorial n = foldr (*) 1 [1..n]

-- n! / k!
factmid :: Int -> Int -> Int
factmid n k = foldr (*) 1 [(k+1)..n]

-- 順列 nPm
permutation :: Int -> Int -> Int
permutation n m = factmid n (n-m)
-- permutation n m = foldr (*) 1 . take m . reverse $ [1..n]

-- 組み合わせ nCk
combination :: Int -> Int -> Int
combination n k = (factmid n k) `div` (factorial k)

-- 重複組み合わせ nHk
hcomb :: Int -> Int -> Int
hcomb n k = (factmid (n + k - 1) (n - 1))`div` factorial k

-- 整数の10進数の桁数
keta :: Int -> Int
keta n = round . (logBase 10) . fromIntegral . head $ filter (> n) tens

tens = map (10^) [0..]
