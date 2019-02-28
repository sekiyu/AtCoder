import Data.List
import Debug.Trace
import Control.Monad

mergeSort :: (Ord a) => [a] -> [a]
mergeSort (a:[]) = [a]
mergeSort as = (mergeSort left) `merge` (mergeSort right)
  where
    (left, right) = splitAt m as
    m = length as `div` 2
    merge :: (Ord a) => [a] -> [a] -> [a]
    merge as [] = as
    merge [] bs = bs
    merge (a:as) (b:bs)
      | a <= b = a:(merge as (b:bs))
      | a > b  = b:(merge (a:as) bs)

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (a:as) = (quickSort smaller) ++ [a] ++ (quickSort larger)
  where
    smaller = filter (< a) as
    larger = filter (>= a) as

selectionSort :: (Ord a) => [a] -> [a]
selectionSort = unfoldr f
  where
    f :: (Ord a) => [a] -> Maybe (a, [a])
    f [] = Nothing
    f as = let m = minimum as in Just (m, delete m as)

bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort = bubble []
  where
    bubble :: (Ord a) => [a] -> [a] -> [a]
    bubble bs [] = reverse bs
    bubble [] (a:as) = bubble [a] as
    bubble (b:bs) (a:as) = if b > a 
                           then bubble bs (a:b:as)
                           else bubble (a:b:bs) as 
                        
insertSort :: (Ord a) => [a] -> [a]
insertSort = foldr insert []
  where
    insert :: (Ord a) => a -> [a] -> [a]
    insert k [] = [k]
    insert k (a:as) = if k < a then k:a:as else a:(insert k as)

shellSort :: (Ord a) => [a] -> [a]
shellSort = shell 4 -- 4 is currently a magic number 
  where
    shell :: (Ord a) => Int -> [a] -> [a]
    shell i = if i == 1
              then insertSort
              else shell (i - 3) . join . transpose . map insertSort . transpose . div i
              
    div :: Int -> [a] -> [[a]]
    div h as = if length as <= h
               then [as]
               else (take h as):(div h $ drop h as)

main = print $ shellSort [3,2,9,6,7,4,1,5]