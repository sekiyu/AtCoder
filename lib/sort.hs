import Data.List
import Debug.Trace
import Control.Monad
import Control.Monad.ST
import Data.Array.Unboxed
import Data.Array.ST

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
bubbleSort = bubble False []
  where
    bubble :: (Ord a) => Bool -> [a] -> [a] -> [a]
    bubble False bs [] = reverse bs
    bubble True bs [] = bubble False [] $ reverse bs
    bubble isSwapped [] (a:as) = bubble isSwapped [a] as
    bubble isSwapped (b:bs) (a:as) = if b > a 
                                     then bubble (True || isSwapped) (b:a:bs) as
                                     else bubble (False || isSwapped) (a:b:bs) as 
              
gnomeSort :: (Ord a) => [a] -> [a]
gnomeSort = gnome []
  where
    gnome :: (Ord a) => [a] -> [a] -> [a]
    gnome bs [] = reverse bs
    gnome [] (a:as) = gnome [a] as
    gnome (b:bs) (a:as) = if b > a 
                          then gnome bs (a:b:as)
                          else gnome (a:b:bs) as 
                        
insertSort :: (Ord a) => [a] -> [a]
insertSort = foldr insert []
{-  where
    insert :: (Ord a) => a -> [a] -> [a]
    insert k [] = [k]
    insert k (a:as) = if k < a 
                      then k:a:as 
                      else a:(insert k as)
-} -- insertはData.Listに定義あり

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

countingSort :: (Int, Int) -> [Int] -> [Int]
countingSort (s,e) as = concatMap (\i -> replicate (c ! i) i) [s..e]
  where
    c :: UArray Int Int
    c = runSTUArray $ do
      arr <- newArray (s,e) 0
      forM_ as (\a -> do
        val <- readArray arr a
        writeArray arr a (val + 1) 
        )
      return arr
               


mainbs = print $ bubbleSort [3,2,9,6,7,4,1,5]
mainis = print $ bubbleSort [3,2,9,6,7,4,1,5]