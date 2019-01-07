import Data.Array

main :: IO ()
main = do
  n <- readLn
  hs <- fmap (map read . words) getLine :: IO [Int]
  print $ solve n hs

solve :: Int -> [Int] -> Int
solve n hs = dp ! n
  where
    harr = listArray (1, n) hs
    dp = listArray (1, n) $ map f [1..n]
    f :: Int -> Int
    f 1 = 0
    f 2 = cost 1 2
    f i = min (dp!(i-1) + cost i (i-1)) (dp!(i-2) + cost i (i-2))
    cost i j = abs (harr!i - harr!j)