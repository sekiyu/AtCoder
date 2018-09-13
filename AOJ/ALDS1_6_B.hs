main :: IO()
main = do
  n <- readLn :: IO Int
  as <- fmap (map read . words) getLine :: IO [Int]
  putStrLn . unwords . map show $ partition' as

partition' :: (Ord a) => [a] -> [a]
partition' as =
  where
    pivot = last as
    divide :: (Ord a) => [a] -> Int -> Int
    divide

partition :: (Ord a) => [a] -> [a]
partition as = [x | x <- init as, x <= pivot] ++ [pivot] ++ [x | x <- init as, x > pivot]
  where pivot = last as
