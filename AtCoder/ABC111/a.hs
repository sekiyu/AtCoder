main :: IO ()
main = do
  n <- getLine
  putStrLn [if x == '1' then '9' else '1' | x <- n ]
