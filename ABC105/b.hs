main :: IO ()
main = do
  n <- readLn :: IO Int
  putStrLn $ if solve n then "Yes" else "No"

solve :: Int -> Bool
solve n = 0 < length [1 | x <- [0..(div n 4)], y <- [0..(div n 7)], 4 * x + 7 * y == n]
