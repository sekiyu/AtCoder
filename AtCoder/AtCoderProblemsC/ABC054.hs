-- ABC054 C One-stroke Path
import Control.Monad
import Data.List
import qualified Data.IntMap.Strict as IntMap

main :: IO ()
main = do
  (n:m:_) <- fmap (map read . words) getLine :: IO [Int]
  am <- replicateM m $ map read . words <$> getLine :: IO [[Int]]
  print $ solve n am

solve :: Int -> [[Int]] -> Int
solve n am = length . filter (\as -> isValid (toAdjList am) (1:as)) $ candidates n

type AdjList = IntMap.IntMap [Int]
toAdjList :: [[Int]] -> AdjList
toAdjList am = go am IntMap.empty
  where
    go :: [[Int]] -> AdjList -> AdjList
    go [] adjList = adjList
    go (s:ab) adjList = go ab $ if a == b then new else new2
      where
        (a:b:_) = s
        new = IntMap.insertWith (++) a [b] adjList
        new2 = IntMap.insertWith (++) b [a] new

candidates :: Int -> [[Int]]
candidates n = permutations [2..n]

isValid :: AdjList -> [Int] -> Bool
isValid adjList as = goAllNodes && validateEdge adjList as
  where
    goAllNodes = all (\i -> i `elem` as) [1..(length as)]
    validateEdge :: AdjList -> [Int] -> Bool
    validateEdge adjList (a:[]) = True
    validateEdge adjList (a:b:as) = b `elem` (adjList IntMap.! a) && validateEdge adjList (b:as)
