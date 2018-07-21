import Control.Monad
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  (n:m:_) <- fmap (map read . words) getLine :: IO [Int]
  gs <- replicateM m $ fmap (map read . words) getLine :: IO [[Int]]
  print . solve $ gs

solve :: [[Int]] -> Int
solve gs = breakBridge 0 . bridges $ gs
  where
    bridges [] = []
    bridges (g:gs) = let (a:b:_) = g in [a..(b - 1)]:bridges gs

    breakBridge :: Int -> [[Int]] -> Int
    breakBridge i [] = i
    breakBridge i bs = breakBridge (i + 1) newb
      where
        counts = toCountMap . join $ bs :: Map.Map Int Int
        (_, bridgeToBreak) = maximum [(v, k) | (k, v) <- Map.toList counts]
        newb = [b | b <- bs, notElem bridgeToBreak b]

toCountMap :: (Ord k) => [k] -> Map.Map k Int
toCountMap xs = Map.fromListWith (+) [(x, 1) | x <- xs]
