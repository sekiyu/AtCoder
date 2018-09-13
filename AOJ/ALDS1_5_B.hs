main :: IO()
main = do
  n <- readLn :: IO Int
  as <- fmap (map read . words) getLine :: IO [Int]
  --let (sorted , n) = mergeSort (as, 0) 0 (n - 1)
  --putStrLn . unwords $ map show sorted
  --print n
  putStrLn . unwords . map show $ mergeSort as

mergeSort :: (Ord a) => [a] -> [a]
mergeSort (a:[]) = [a]
mergeSort as
  | left + 1 < right = merge (mergeSort (slice left (mid - 1) as)) (mergeSort  (slice mid right as))
  | otherwise = [as !! left]
    where
      left = 0
      right = length as
      mid = (left + right) `div` 2

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

merge :: (Ord a) => [a] -> [a] -> [a]
merge as [] = as
merge [] bs = bs
merge (a:as) (b:bs)
  | a <= b = a:(merge as (b:bs))
  | a > b  = b:(merge (a:as) bs)
