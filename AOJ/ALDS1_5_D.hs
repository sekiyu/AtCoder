main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- fmap (map read . words) getLine :: IO [Int]
  print $ solve as



-- naive solution
solve [] = 0
solve (a:as) = count a as + solve as

count _ [] = 0
count k (a:as) = (if k < a then 0 else 1) + count k as