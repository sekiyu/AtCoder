-- AB119 C
import Control.Monad

main :: IO ()
main = do
  (n:a:b:c:_) <- fmap (map read . words) getLine :: IO [Int]
  ls <- replicateM n $ readLn :: IO [Int]
  print $ solve n (a,b,c) ls

solve n abc ls = minimum [ totalCost abc ls c | c <- replicateM n "abcn"]

totalCost :: (Int, Int, Int) -> [Int] -> String -> Int
totalCost (a,b,c) ls candidate = as + bs + cs
  where
    as = cost a . map fst . filter (\(_, x) -> x == 'a') $ zip ls candidate
    bs = cost b . map fst . filter (\(_, x) -> x == 'b') $ zip ls candidate
    cs = cost c . map fst . filter (\(_, x) -> x == 'c') $ zip ls candidate

cost _ [] = maxBound `div` 10 :: Int
cost a ls = 10 * length ls + abs (a - sum ls) - 10