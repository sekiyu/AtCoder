-- ABC110 B 1 Dimensional World's Tale
main :: IO ()
main = do
  (n:m:x:y:_) <- fmap (map read . words) getLine :: IO [Int]
  xs <- fmap (map read . words) getLine :: IO [Int]
  ys <- fmap (map read . words) getLine :: IO [Int]
  putStrLn $ if war (x:xs) (y:ys)
             then "War"
             else "No War"

war :: [Int] -> [Int] -> Bool
war xs ys = (maximum xs) >= (minimum ys)
