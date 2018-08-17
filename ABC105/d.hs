import qualified Data.IntMap.Strict as IntMap


main :: IO ()
main = do
  (n:m:_) <- fmap (map read . words) getLine :: IO [Int]
  as <- fmap (map read . words) getLine :: IO [Int]
  print $ solve n m as

solve :: Int -> Int -> [Int] -> Int
solve n m as = innerSolve IntMap.empty as m
  where
    innerSolve :: IntMap.IntMap Int -> [Int] -> Int -> Int
    innerSolve map [] _ = IntMap.findWithDefault 0 0 map
