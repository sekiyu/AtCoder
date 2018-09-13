import qualified Data.IntSet as IntSet
import           Data.Maybe  (fromJust)

main :: IO ()
main = do
  n <- readLn
  print $ solve' n

-- 幅優先探索
-- 探索の深さだけ覚えていれば良いのでIntSetを使った
solve :: Int -> Int
solve n = bfs 0 $ IntSet.singleton n
  where
    bfs i visited
      | IntSet.member 0 visited = i
      | otherwise = bfs (i + 1) $ visit candidates visited

    visit :: [Int] -> IntSet.IntSet -> IntSet.IntSet
    visit as vs = IntSet.fromList $ [v - a | a <- as, v <- (IntSet.elems vs), v - a >= 0]

    candidates = a ++ b ++ c
    a = [1]
    b = filter (<= 10^5) (map (6^) [1..6])
    c = filter (<= 10^5) (map (9^) [1..9])

-- 別解　Nを6進数と9進数の和で表現するパターンを全探索
solve' :: Int -> Int
solve' n = minimum $ map sum $ [(ndigits 6 m) ++ (ndigits 9 (n - m)) | m <- [0..n]]

ndigits :: Int -> Int -> [Int]
ndigits k n = reverse $ inner n k
  where
    inner 0 _ = []
    inner n k = (n `mod` k):inner (n `div` k) k
