import Data.List
import Data.Monoid
import Control.Monad.Writer

main :: IO()
main = do
  n <- readLn :: IO Int
  as <- fmap (map read . words) getLine :: IO [Int]
  --let (sorted , n) = mergeSort (as, 0) 0 (n - 1)
  --putStrLn . unwords $ map show sorted
  -- putStrLn . unwords . map show $ mergeSort as
  -- putStrLn . unwords . map show $ sort as

  let (sorted, n) = runWriter $ mergeSortWithCount as
  putStrLn . unwords . map show $ sorted
  print $ getSum n
  
-- solve :: [Int] -> Int
-- solve as = numCompMergeSort $ transpose [as]

{-
This algorithm is different from that of quasi-code in the problem.
Therefore, the nnumber of comparison is not correct.
-}
mergeSortWithCount :: (Ord a) => [a] -> Writer (Sum Int) [a]
mergeSortWithCount as = fmap join $ return (transpose [as]) >>= foldr (<=<) return (replicate n go'')
  -- go . map return $ transpose [as]
  where
    n = 1 + (round . logBase 2 . fromIntegral $ length as)
    go'' :: (Ord a) => [[a]] -> Writer (Sum Int) [[a]]
    go'' as = writer (xs, Sum n)
      where
        tuples = map runWriter $ innerGo as
        xs = foldr (\x acc -> (fst x):acc) [] tuples
        n = sum . map (getSum . snd) $ tuples :: Int
        
        innerGo :: (Ord a) => [[a]] -> [Writer (Sum Int) [a]]
        innerGo [] = []
        innerGo (as:[]) = [return as]
        innerGo (as:bs:cs) = (mergeM as bs):(innerGo cs)

    go :: (Ord a) => [Writer (Sum Int) [a]] -> Writer (Sum Int) [a]
    go as | length as == 1 = head as
          | otherwise = go $ go' as

    go' :: (Ord a) => [Writer (Sum Int) [a]] -> [Writer (Sum Int) [a]]
    go' [] = []
    go' (x:[]) = [x]
    go' (as:bs:cs) = (liftM2 merge as bs):(go' cs)
    
        

-- 引数のリスト2つはソート済みと想定
mergeM :: (Ord a) => [a] -> [a] -> Writer (Sum Int) [a]
mergeM [] bs = return bs
mergeM as [] = return as -- writer (as, Sum (length as))
mergeM (a:as) (b:bs) = if a < b
                       then mergeM as (b:bs) >>= (consCount a)
                       else mergeM (a:as) bs >>= (consCount b)

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] bs = bs
merge as [] = as
merge (a:as) (b:bs) = if a < b
                      then a:(merge as (b:bs))
                      else b:(merge (a:as) bs)
                       

consCount :: a -> [a] -> Writer (Sum Int) [a]
consCount a as = writer (a:as, 1)

consNotCount :: a -> [a] -> Writer (Sum Int) [a]
consNotCount a as = writer (a:as, 0)

appendNotCount :: [a] -> [a] -> Writer (Sum Int) [a]
appendNotCount as bs = writer (as ++ bs, 0)


{-
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
-}