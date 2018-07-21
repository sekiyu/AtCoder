main :: IO ()
main = do
  s <- getLine
  t <- getLine
  putStrLn $ solve s t 0

solve :: String -> String -> Int -> String
solve s t i
  | i > length s = "No"
  | otherwise = if s == t
                then "Yes"
                else solve news t (i + 1)
    where news = [last s] ++ init s
