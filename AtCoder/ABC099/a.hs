--- ABC099 A
main :: IO ()
main = do
  n <- readLn
  putStrLn $ solve n

solve n 
  | n > 999 = "ABD"
  | otherwise = "ABC"
