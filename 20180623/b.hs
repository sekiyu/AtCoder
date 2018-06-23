main :: IO()
main = do
  ss <- getLine :: IO String
  putStrLn $ if solve ss then "Yes" else "No"

solve :: String -> Bool
solve ss = mod (read ss) sn == 0
  where
    sn = f ss 0
    f :: String -> Int -> Int
    f [] n = n
    f (s:ss) n = f ss (read [s] + n)
