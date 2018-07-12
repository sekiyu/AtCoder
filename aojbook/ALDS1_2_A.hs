{-# LANGUAGE BangPatterns #-}

main :: IO ()
main = do
  _ <- getLine
  as <- fmap (map read . words) getLine :: IO [Int]
  solve as

solve :: (Ord a, Show a) => [a] -> IO ()
solve as = bubbleSort [] as 0
  where
    bubbleSort :: (Ord a, Show a) => [a] -> [a] -> Int -> IO()
    bubbleSort sorted [] n = do
      putStrLn . unwords . map show . reverse $ sorted
      print n
    bubbleSort [] (b:bs) i = bubbleSort [b] bs i
    bubbleSort (a:as) (b:bs) i
      | a < b     = bubbleSort (b:a:as) bs i
      | otherwise = bubbleSort as (b:a:bs) (i + 1)

bubblesort :: (Ord a) => [a] -> [a]
bubblesort [] = []
bubblesort xs = x:bubblesort ys
    where
        x:ys = foldr bubble [] xs
        bubble :: (Ord a) => a -> [a] -> [a]
        bubble p [] = [p]
        bubble p (q:ps)
            | q < p     = q:p:ps
            | otherwise = p:q:ps

bubbleSort' :: (Ord a) => [a] -> [a]
bubbleSort' as = reverse . bubble $ reverse as
  where
    bubble :: (Ord a) => [a] -> [a]
    bubble [x] = [x]
    bubble (a:b:as)
      | a < b     = b:(bubble $ a:as)
      | otherwise = a:(bubble $ b:as)
