{-# LANGUAGE BangPatterns #-}

main :: IO ()
main = do
  _ <- getLine
  as <- fmap (map read . words) getLine :: IO [Int]
  solve as

solve :: [Int] -> IO ()
solve as = bubbleSort [] as 0
  where
    bubbleSort :: [Int] -> [Int] -> Int -> IO()
    bubbleSort sorted [] n = do
      putStrLn . unwords . map show . reverse $ sorted
      print n
    bubbleSort [] (b:bs) i = bubbleSort [b] bs i
    bubbleSort (a:as) (b:bs) i
      | a < b = bubbleSort (b:a:as) bs i
      | otherwise = bubbleSort [] (reverse as ++ b:a:bs) (i + 1)
