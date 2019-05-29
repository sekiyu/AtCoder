{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Data.List

main :: IO ()
main = do
  (n:m:_) <- fmap (map read . words) getLine :: IO [Int]
  as <- fmap (map read . words) getLine :: IO [Integer]
  bcs <- replicateM m $ (\(a:b:_) -> (a,b)) . map read . words <$> getLine :: IO [(Integer, Integer)]
  print $ solve as bcs

solve as bcs = go 0 (sort as) (reverse $ sortOn snd bcs)
  where
    go s [] _ = s
    go s as [] = s + sum as
    go s (a:as) ((b, c):bcs) = 
      if b == 0 
      then go s (a:as) bcs
      else if a < c
           then go (s + c) as ((b-1, c):bcs)
           else s + a + sum as 
