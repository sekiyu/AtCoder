main :: IO ()
main = do
  n <- readLn
  print $ solve n

solve :: Int -> Int
solve n = head [ 111 * x | x <- [1..9], 111 * x >= n ]
