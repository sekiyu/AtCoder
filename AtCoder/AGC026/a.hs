main :: IO ()
main = do
  _ <- getLine
  as <- fmap (map read . words) getLine :: IO [Int]
  print . solve $ as

solve :: [Int] -> Int
solve (a:b:[]) = if a == b then 1 else 0
solve (a:b:c:as)
  | a == b = if b == c
    then (1 + solve (c:as))
    else (1 + solve (b:c:as))
  | otherwise = solve $ b:c:as
solve _ = 0
