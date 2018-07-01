import Data.List

main = do
  n <- readLn :: IO Int
  as <- fmap (map read . words) getLine :: IO [Int]
  print . solve . sort $ zipWith (\x y -> x - y) as [1..]

snk :: [Int] -> Int -> Int
snk as b = sum $ map (\a -> abs(a - b)) as

solve :: [Int] -> Int
solve bs = minimum $ map (snk bs) candidates
  where
    n = length bs
    optb = bs !! (div n 2)
    candidates = [(optb - 1)..(optb + 1)]

--count as b = sum [if x - b > 0 then 1 else -1 | x <- as]
count as b = length (filter (>b) as) - div (length as) 2
binarySearch bs i = 1
