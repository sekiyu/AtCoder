import qualified Data.IntSet as IntSet

main :: IO ()
main = do
  n <- readLn :: IO Int
  ss <- fmap (map read . words) getLine :: IO [Int]
  print $ solve ss

solve :: [Int] -> Int
solve ss = go ss $ IntSet.singleton 0
  where
    go [] set = IntSet.size set
    go (s:ss) set = go ss $ IntSet.union set (IntSet.map (+s) set)
