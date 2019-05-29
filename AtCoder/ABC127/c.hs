{-# LANGUAGE BangPatterns #-}
import Control.Monad

main :: IO ()
main = do
  (n:m:_) <- fmap (map read . words) getLine :: IO [Int]
  lrs <- replicateM m $ (\(a:b:_) -> (a,b)) . map read . words <$> getLine :: IO [(Int, Int)]
  print $ solve n lrs

solve n lrs = max 0 $ rmin - lmx + 1
  where 
    lmx = maximum $ map fst lrs
    rmin = minimum $ map snd lrs