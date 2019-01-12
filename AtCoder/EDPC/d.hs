import Control.Monad
import Data.Array

main :: IO ()
main = do
  (n:w:_) <- fmap (map read . words) getLine :: IO [Int]
  wvs <- replicateM n $ (\(x:y:_) -> (x, y)) . map read . words <$> getLine :: IO [(Int, Int)]
  print $ solve n w wvs

solve :: Int -> Int -> [(Int, Int)] -> Int
solve n w wvs = dp!(n, w)
  where
    wvarr = listArray (1, n) wvs
    dp = listArray ((0, 0), (n, w)) $ map f [(i, j) | i <- [0..n], j <- [0..w]]
    f :: (Int, Int) -> Int
    f (_, 0) = 0
    f (0, _) = 0
    f (i, j) 
      | j - wi >= 0 = max (dp!(i-1, j-wi) + vi) (dp!(i-1, j))
      | otherwise = dp!(i-1, j)
      where 
        wi = fst (wvarr!i)
        vi = snd (wvarr!i)