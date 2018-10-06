main :: IO ()
main = do
  (n:m:_) <- fmap (map read . words) getLine :: IO [Int]
  print $ solve n m

solve :: Int -> Int -> Int
solve n m = go (m `div` n) n m
  where
    go i n m
      | m `mod` i == 0 = if check n (m `div` i)
                         then i
                         else go (i+1) n m
      | otherwise = go (i-1) n m
    check n m = True
