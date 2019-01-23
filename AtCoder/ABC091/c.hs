-- ABC091 C
import Control.Monad
import Data.Maybe
import Data.List
import Data.Array

main :: IO ()
main = do
  n <- readLn
  xs <- replicateM n $ map read . words <$> getLine :: IO [[Int]]
  ys <- replicateM n $ map read . words <$> getLine :: IO [[Int]]
  -- print $ solve xs ys
  print $ solve'' n xs ys

solve'' :: Int -> [[Int]] -> [[Int]] -> Int
solve'' n xs ys = n - (length $ foldl' go blues reds)
  where
    reds = reverse . sortOn last $  xs
    blues = sortOn head $ ys
    go :: [[Int]] -> [Int] -> [[Int]]
    go bs r = deleteBy canBeGoodFriends r bs

canBeGoodFriends :: [Int] -> [Int] -> Bool
canBeGoodFriends (a:b:_) (c:d:_) = a < c && b < d
    

solve :: [[Int]] -> [[Int]] -> Int
solve xs ys = go (reverse $ sort xs) (reverse $ sort ys)
  where
    go :: [[Int]] -> [[Int]] -> Int
    go [] _ = 0
    go (a:as) cs
      | isNothing found = go as cs
      | otherwise = 1 + go as (delete (fromJust found) cs)
      where
        found = find a cs

    find :: [Int] -> [[Int]] -> Maybe [Int]
    find a cs
      | null candidates = Nothing
      | otherwise = Just (head candidates)
      where
        candidates = filter (canBeGoodFriends a) cs

solve' :: Int -> [[Int]] -> [[Int]] -> Int
solve' n xs ys = 0
  where
    reds = listArray (1, n) xs
    blues = listArray (1, n) ys
    canBeGoodFriends :: Int -> Int -> Bool
    canBeGoodFriends i j = a < c && b < d
      where 
        (a:b:_) = reds!i
        (c:d:_) = blues!j


        