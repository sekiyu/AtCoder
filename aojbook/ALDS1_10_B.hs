import Control.Monad

main :: IO()
main = do
  n <- readLn
  ms <- replicateM n $ fmap (map read . words) getLine :: IO [[Int]]
  print . solve $ intoNums ms
  --print $ intoNums ms

intoNums :: [[Int]] -> [Int]
intoNums (m:[]) = let (a:b:_) = m in [a, b]
intoNums (m:ms) = a:(intoNums ms)
  where (a:b:_) = m

solve :: [Int] -> Int
solve (a:[]) = 0
solve (a:b:[]) = 0
solve (a:b:c:[]) = a * b * c
solve ms = minimum [(solve (take i ms)) + (solve (drop i ms)) + v i
                    | i <- [1, (n - 1)], not (i == 2 && n == 4) ]
  where
    n = length ms
    v i = (head ms) * (last ms) * (ms !! (i))
