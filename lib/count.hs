import Data.List


-- 階乗
factorial :: Int -> Int
factorial n = foldr (*) 1 [1..n]

-- 組み合わせ
combination :: Int -> Int -> Int
combination m i = (factorial m) `div` (factorial i) `div` (factorial (m - i))

-- 順列
permutation :: Int -> Int -> Int
permutation n m = foldr (*) 1 . take m . reverse $ [1..n]

-- 整数の10進数の桁数
keta :: Int -> Int
keta n = round . (logBase 10) . fromIntegral . head $ filter (> n) tens

tens = map (10^) [0..]
