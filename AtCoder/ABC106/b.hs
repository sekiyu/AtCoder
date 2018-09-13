main :: IO ()
main = do
  n <- readLn :: IO Int
  print $ solve n

solve :: Int -> Int
solve n
  | n < 3 * 5 * 7 = 0
  | n < 3 ^ 3 * 5 = 1
  | n < 3 * 5 * 11 = 2
  | n < 3 ^ 3 * 7 = 3
  | n < 3 * 5 * 13 = 4
  | otherwise = 5
