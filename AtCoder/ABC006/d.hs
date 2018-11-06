-- ABC006 D - トランプ挿入ソート
import Control.Monad
import Data.Array
import Data.List

main :: IO ()
main = do
  n <- readLn
  cs <- join <$> (replicateM n $ map read . words <$> getLine) :: IO [Int]
  -- print $ solve2 n cs
  print $ partial2 n cs


partial1 :: Int -> [Int] -> Int
partial1 n cs = n - (maximum . map length . filter validate . subsequences $ cs)
  where
    validate c = sort c == c

partial2 :: Int -> [Int] -> Int
partial2 n cs = n - (find [] cs)
  where
    find :: [(Int, Int)] -> [Int] -> Int
    find bs [] = maximum . map snd $ bs
    find [] (c:cs) = find [(c, 1)] cs
    find bs (c:cs) = find ((minMax c bs):bs) cs

    minMax c bs = go 0 c bs
      where
        go m _ [] = (c, m + 1)
        go m c (b:bs)
          | c > fst b = go (max m $ snd b) c bs
          | otherwise = go m c bs

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
