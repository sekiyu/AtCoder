-- ABC006 D - トランプ挿入ソート
import Control.Monad
import Data.Array
import Data.List

main :: IO ()
main = do
  n <- readLn
  cs <- join <$> (replicateM n $ map read . words <$> getLine) :: IO [Int]
  print $ solve2 n cs


solve2 :: Int -> [Int] -> Int
solve2 n cs = fst $ foldl' f (0, 0) cs
  where
    f :: (Int, Int) -> Int -> (Int, Int)
    f (i, cmax) c
      | c > cmax = (i, c)
      | otherwise = (i+1, cmax)

solve :: Int -> [Int] -> Int
solve n cs = fst $ dp!n
  where
    carray = listArray (1, n) cs
    dp :: Array Int (Int, Int)
    dp = listArray (1, n) $ map f [1..n]
    f 1 = (0, carray!1)
    f i | snd prev < carray!i = (fst prev, carray!i)
        | otherwise = (fst prev + 1, snd prev)
        where
          prev = dp!(i-1)
