import Control.Monad
import qualified Data.Map.Strict as Map
import Data.List
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
readInts = map (fst . fromJust . B.readInt) . B.words <$> B.getLine

main :: IO ()
main = do
  (n:m:_) <- fmap (map read . words) getLine :: IO [Int]
  -- gs <- replicateM m $ fmap (map read . words) getLine :: IO [[Int]]
  gs <- replicateM m $ readInts
  print . solve' 0 . sortBridge $ gs

sortBridge = sortBy (\x y -> (x !! 1) `compare` (y !! 1))
isDivided i ds = (ds !! 0) <= i && (ds !! 1) > i

solve' :: Int -> [[Int]] -> Int
solve' i [] = i
solve' i gs = solve' (i + 1) $ filter (not . isDivided bridge) gs
  where
    bridge = (head gs) !! 1 - 1



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
