main :: IO ()
main = do
  n <- readLn :: IO Int
  a <- getLine :: IO String
  b <- getLine :: IO String
  c <- getLine :: IO String
  print $ solve a b c

solve :: String -> String -> String -> Int
solve [] _ _ = 0
solve (a:as) (b:bs) (c:cs) = i + solve as bs cs
  where
    i = if a == b && b == c
        then 0
        else if a == b || b == c || c == a
              then 1
              else 2