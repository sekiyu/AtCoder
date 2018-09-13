import Control.Monad
import Data.Functor

main :: IO ()
main = do
  (n:m:_) <- fmap (map read . words) getLine :: IO [Int]
  -- ps <- fmap join . replicateM n $ map read . words <$> getLine :: IO [Int]
  ps <- replicateM n $ readLn :: IO [Int]
  solve n m ps

solve n m ps
  | n == 0 && m == 0 = return ()
  | otherwise = do
      -- print . naiveSolve m . join $ ps
      print . naiveSolve m $ ps
      main

naiveSolve :: Int -> [Int] -> Int
naiveSolve m ps = maximum . filter (<=m) . map sum $ candidates
  where
    candidates = join . map (flip replicateM ps) $ [0..4]
