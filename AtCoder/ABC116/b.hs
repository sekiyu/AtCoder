-- ABC116 B
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import Debug.Trace
import Data.Array

main :: IO ()
main = do
  s <- readLn :: IO Integer
  print $ solve s

-- solve :: Int -> Int
solve s = find Set.empty [1..]
  where
    a = listArray (1, 1000000) $ map g [1..1000000]
    f n | n `mod` 2 == 0 = n `div` 2
        | otherwise = 3 * n + 1
    g 1 = s
    g i = f $ a!(i-1)

    -- find :: Set.Set Integer -> [Int] -> Int
    find as (i:is)
      | Set.member ai as = i
      | otherwise = find (Set.insert ai as) is
        where ai = a!i



