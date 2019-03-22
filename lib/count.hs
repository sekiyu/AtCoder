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

-- 2進数への変換
intToBin :: Int -> [Int]
intToBin = unfoldr (\x ->
        if x == 0
        then Nothing
        else Just (mod x 2, div x 2))

-- 2進数への変換(2)
toBinary :: Int -> [Int]
toBinary = reverse . go
  where
    go 0 = []
    go !n = (n `mod` 2):(go (n `div` 2))


toCountMap :: (Ord k) => [k] -> Map.Map k Int
toCountMap = Map.fromListWith (+) . flip zip (repeat 1)
        