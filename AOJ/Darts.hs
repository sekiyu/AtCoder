import Control.Monad
import Data.Functor

main :: IO ()
main = do
  (n:m:_) <- fmap (map read . words) getLine :: IO [Int]
  ps <- replicateM n $ map read . words <$> getLine :: IO [[Int]]
  solve0 n m ps

solve n m ps
  | n == 0 && m == 0 = return ()
  | otherwise = do
      print . solve m . join $ ps
      main

naiveSolve :: Int -> [Int] -> Int
naiveSolve m ps = maximum . filter (<=m) . map sum $ candidates
  where
    candidates = join . map (flip replicateM ps) $ [0..4]
