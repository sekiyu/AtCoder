main :: IO ()
main = do
  _ <- getLine
  ts <- map read . words <$> getLine :: IO [Int]
  solve ts 1

solve :: [Int] -> Int -> IO ()
solve tree i
  | i > length tree = return ()
  | otherwise = do
      putStrLn . unwords $ filter (\x -> length x > 0) ans
      solve tree (i + 1)
        where
          parent = div i 2
          left = i * 2
          right = i * 2 + 1
          ans =
            ["Node " ++ (show i) ++ ":",
            "key = " ++ (show $ tree !! (i - 1)) ++ ",",
            if parent > 0 then "parent key = " ++ (show $ tree !! (parent - 1)) ++ "," else "",
            if left <= length tree then "left key = " ++ (show $ tree !! (left - 1)) ++ "," else "",
            if right <= length tree then "right key = " ++ (show $ tree !! (right - 1)) ++ "," else ""]
