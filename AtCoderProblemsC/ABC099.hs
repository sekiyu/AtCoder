import qualified Data.IntSet as IntSet
import           Data.Maybe  (fromJust)

main :: IO ()
main = do
  n <- readLn
--  print $ solve n 0
  print $ solve'' n

solve :: Int -> Int -> Int
solve 0 i = i
solve n i = solve (n - k) (i + 1)
  where
   k = maximum . filter (n >= ) $ candidates

candidates = a ++ b ++ c
a = [1]
b = filter (<= 10^5) (map (6^) [1..6])
c = filter (<= 10^5) (map (9^) [1..9])

solve' :: Int -> Int
solve' 0 = 0
solve' n
  | elem n candidates = 1
  | otherwise = 1 + (solve' (last next))
  -- | otherwise = 1 + (minimum $ map solve' next)
    where
      next = filter (0 <) . map (n - ) $ candidates
-- maximum . map (\m -> 1 + solve' m) . filter (0 <) . map (n - ) $ candidates

solve'' :: Int -> Int
solve'' n = bfs 0 $ IntSet.singleton n
  where
    bfs i visited
      | IntSet.member 0 visited = i
      | otherwise = bfs (i + 1) $ visit candidates visited

    visit :: [Int] -> IntSet.IntSet -> IntSet.IntSet
    visit as vs = IntSet.fromList $ [v - a | a <- as, v <- (IntSet.elems vs), v - a >= 0]
