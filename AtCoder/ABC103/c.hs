main :: IO ()
main = do
  _ <- getLine
  as <- fmap (map read . words) getLine :: IO [Int]
  print . solve $ as

solve :: [Int] -> Int
solve as = sum as - length as
