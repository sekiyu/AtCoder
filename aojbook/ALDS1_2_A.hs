{-# LANGUAGE BangPatterns #-}

main :: IO ()
main = do
  _ <- getLine
  as <- fmap (map read . words) getLine :: IO [Int]
  let (sorted, n) = bubbleSortC as
  putStrLn . unwords . map show $ sorted
  print n
  --solve as

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

bubbleSortC :: (Ord a) => [a] -> ([a], Int)
bubbleSortC [] = ([], 0)
bubbleSortC as = cons' (x, i) $ bubbleSortC $ reverse xs
  where
    ((x:xs), i) = foldr bubble ([], 0) as

    bubble :: (Ord a) => a -> ([a], Int) -> ([a], Int)
    bubble x ([], i) = ([x], i)
    bubble x ((a:as), i)
      | a < x     = (a:x:as, i + 1)
      | otherwise = (x:a:as, i)

    cons' :: (Ord a) => (a, Int) -> ([a], Int) -> ([a], Int)
    cons' a b = let (x, i) = a
                    (xs, j) = b
                in (x:xs, i + j)
