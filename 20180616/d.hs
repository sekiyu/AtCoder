import Data.List
import Control.Monad

main :: IO()
main = do
  [n, m] <- map read . words <$> getLine :: IO([Int])
  ds <- replicateM n $ map read . words <$> getLine :: IO([[Int]])
  print $ maximum [solve (map (zipWith (*) [i, j, k]) ds ) m | i <- [1, -1], j<-[1, -1], k<- [1,-1]]

solve :: [[Int]] -> Int -> Int
solve ds m = sum . take m . reverse $ sort [sum d | d <- ds]
