import Control.Monad
import qualified Data.IntSet as IntSet
import Data.Maybe

main :: IO ()
main = do
  (n:m:_) <- fmap (map read . words) getLine :: IO [Int]
  uvs <- replicateM m $ map read . words <$> getLine :: IO [[Int]]
  print $ solve [1..n] uvs

solve :: [Int] -> [[Int]] -> Int
solve [] uvs = 0
solve (u:unvisiteds) uvs = if isClosed then 0 else 1 + solve new uvs
  where
    (isClosed, visiteds) = isWithinClosed [u] uvs IntSet.empty
    new = filter (\i -> IntSet.notMember i visiteds) unvisiteds

    -- 幅優先探索で閉路か否か判定
    -- 途中通ったノードとエッジは削除していく
    isWithinClosed :: [Int] -> [[Int]] -> IntSet.IntSet -> (Bool, IntSet.IntSet)
    isWithinClosed [] _ visited = (False, visited)
    isWithinClosed (u:us) uvs visited
      | isClosed = (True, visited)
      | otherwise = isWithinClosed (vs ++ us) newUvs newVisited
      where
        (vs, newUvs) = visit u uvs
        isClosed = any (`IntSet.member` visited) vs
        newVisited = foldr IntSet.insert visited vs

    -- 対象のノードと繋がっているノードと、それらへ行くために使ったエッジを削除したエッジ群を返す
    visit :: Int -> [[Int]] -> ([Int], [[Int]])
    visit u uvs = (visited, unusedEdge)
      where
        visited = catMaybes $ map (\(l:r:_) ->
          if l == u
          then Just r
          else if r == u
               then Just l
               else Nothing) uvs
        unusedEdge = filter (notElem u) uvs
