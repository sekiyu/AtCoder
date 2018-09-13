import qualified Data.IntMap.Strict as IntMap


main :: IO ()
main = do
  (n:m:_) <- fmap (map read . words) getLine :: IO [Int]
  as <- fmap (map read . words) getLine :: IO [Int]
  print $ solve n m as

solve :: Int -> Int -> [Int] -> Int
solve n m as = innerSolve (emptyMap m) as m
  where
    innerSolve :: IntMap.IntMap Int -> [Int] -> Int -> Int -> Int
    innerSolve mp [] _ = IntMap.findWithDefault 0 0 map
    innerSolve mp (a:as) m = innerSolve newmp as m
      where
        d = a `mod` m
        newmp = if d == 0
          then map (*2) mp
          else IntMap.foldrWithKey (\k v m -> ) emptyMap mp

emptyMap m = IntMap.fromList $ zip [0..(m - 1)] $ repeat 0
