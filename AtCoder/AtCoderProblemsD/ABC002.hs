import Control.Monad
import qualified Data.IntMap.Strict as IntMap
import Data.List

main :: IO ()
main = do
  (n:m:_) <- fmap (map read . words) getLine :: IO [Int]
  am <- replicateM m $ map read . words <$> getLine :: IO [[Int]]
  print $ solve n am

solve :: Int -> [[Int]] -> Int
solve n am = length . head . filter (isHabatsu am) . sortedCandidates $ n
-- solve n am = filter (isHabatsu am) . sortedCandidates $ n
-- solve n am = sortedCandidates $ n

sortedCandidates :: Int -> [[Int]]
sortedCandidates n = reverse . sortBy (\x y -> compare (length x) (length y)) $ subsequences [1..n]

isHabatsu :: [[Int]] -> [Int] -> Bool
isHabatsu relations candidate = all (flip elem relations) $ edges candidate

edges :: [Int] -> [[Int]]
edges candidate = filter (\(a:b:_) -> a < b) $ replicateM 2 candidate
