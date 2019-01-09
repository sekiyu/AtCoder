import Data.Array

main :: IO ()
main = do
  (n:k:_) <- fmap (map read . words) getLine :: IO [Int]
  hs <- fmap (map read . words) getLine :: IO [Int]
  print $ solve n k hs

solve :: Int -> Int -> [Int] -> Int
solve n k hs = dp ! n
  where
    harr = listArray (1, n) hs
    dp = listArray (1, n) $ map f [1..n]
    f :: Int -> Int
    f 1 = 0
    f i = minimum $ map g [(max 1 (i - k))..(i - 1)]
      where g j = dp!j + cost i j
    cost i j = abs (harr!i - harr!j)
