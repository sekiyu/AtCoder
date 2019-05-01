main :: IO ()
main = do
  s <- getLine :: IO String
  k <- readLn :: IO Int
  putStrLn $ solve s k

solve :: String -> Int -> String
solve s k = foldr (.) id (replicate k' f) $ s
  where
    k' = k `mod` length s
    f (s:ss) = ss ++ [s]
