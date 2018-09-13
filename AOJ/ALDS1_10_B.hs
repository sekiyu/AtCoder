import Control.Monad

main :: IO()
main = do
  n <- readLn
  ms <- replicateM n $ fmap (map read . words) getLine :: IO [[Int]]
  --print . solve . intoNums $ ms
  --print $ intoNums ms
  print . solve2 [] . intoNums $ ms

intoNums :: [[Int]] -> [Int]
intoNums (m:[]) = let (a:b:_) = m in [a, b]
intoNums (m:ms) = a:(intoNums ms)
  where (a:b:_) = m

solve :: [Int] -> Int
--solve (a:[]) = 0
solve (a:b:[]) = 0
solve (a:b:c:[]) = a * b * c
solve ms = minimum [(solve $ take i ms) + (solve $ drop (i - 1) ms) + v i
                    | i <- [2..(n - 1)]]
  where
    n = length ms
    v i = (head ms) * (last ms) * (ms !! (i - 1))

solve2 :: [[Int]] -> [Int] -> Int
solve2 ms as
  | m == n = head . head $ ms
  | m == 0 = solve2 [repeat 0] as
  | otherwise = solve2 ((innerSolve m as ms):ms) as
    where
      n = length as
      m = length ms

innerSolve :: Int -> [Int] -> [[Int]] -> [Int]
innerSolve 1 as _ = (product . take 3 $ as):(innerSolve 1 (drop 1 as)  _)
innerSolve m as ms = (cost $ take m as):(innerSolve m (drop (m - 1) as) ms)
minimum [ memo i + memo i + v i | i <- [2..(n - 1)]]
  where
    n = length as
