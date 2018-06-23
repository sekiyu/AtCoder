main :: IO()
main = do
  ss <-getLine :: IO String
  print $ solve ss 0

solve :: String -> Int -> Int
solve [] n = n
solve ('+':ss) n = solve ss $ n + 1
solve ('-':ss) n = solve ss $ n - 1
