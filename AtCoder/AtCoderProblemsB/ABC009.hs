main :: IO ()
main = do
  s <- getLine
  putStrLn $ solve s

solve :: String -> String
solve "a" = "-1"
solve _ = "a"
