import qualified Data.Map.Strict as Map
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import           Data.Maybe  (fromJust)
import Data.Array


-- 有向グラフ問題でBFS ALDS1_11_C
-- 隣接行列を取り、各ノードまでの最短経路の距離をMapで返す
type Graph = [[Int]]
type AdjMatrix = [[Bool]]
bfs :: AdjMatrix -> [Int] -> Map.Map Int Int -> Map.Map Int Int
bfs _ [] vs = vs
bfs gs qs vs = bfs gs (adjs ++ init qs) newvs
  where
    q = last qs
    edges = gs !! q
    adjs = [i | i <- [0..(length gs - 1)], edges !! i, notElem i $ Map.keys vs]
    Just time = Map.lookup q vs
    newvs = foldr (\adj vs -> Map.insert adj (time + 1) vs) vs adjs

toAdjMatrix :: Graph -> AdjMatrix
toAdjMatrix gs = scanColumn gs $ length gs
  where
    scanColumn :: Graph -> Int -> AdjMatrix
    scanColumn [] _ = []
    scanColumn (x:xs) n = (makeRow x n) : (scanColumn xs n)

    makeRow :: [Int] -> Int -> [Bool]
    makeRow (u:k:vs) n = [if x `elem` vs then True else False | x <- [1..n]]



--　ARC005 C
-- 二次元平面で到達可能な点をBFSで探索
{-
type Position = (Int, Int)
type Maze = Array Position Bool
bfs2D :: Set.Set Position -> Set.Set Position -> Array Position Int -> Array Position Int
bfs2D visiting visited rc
  | visiting == Set.empty = rc -- 訪れた位置のrcを更新
  | otherwise = bfs2D visit (Set.union visited visit) (n + 1)
    where
      adjacents = Set.fromList . join . map step $ Set.toList visiting
      visit = adjacents Set.\\ visited

step :: Position -> [Position]
step (x,y) = validate [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

validate :: [Position] -> [Position]
validate = filter (\p -> isInRange p (bounds maze) && maze!p)

isInRange :: Position -> (Position, Position) -> Bool
isInRange p (l, u) = lx <= px
                    && px <= ux
                    && ly <= py
                    && py <= uy
  where
    (px,py) = p
    (lx,ly) = l
    (ux,uy) = u
-}

-- ABC099 C
-- 一回の操作で引き出せる金額を全部列挙し、1ステップ毎に全ての選択肢を試すbfs
-- 何ステップで到達できたかだけ分かればよいため引数が少ない
solve :: Int -> Int
solve n = bfs 0 $ IntSet.singleton n
  where
    bfs :: Int -> IntSet.IntSet -> Int
    bfs i visited
      | IntSet.member 0 visited = i
      | otherwise = bfs (i + 1) $ visit candidates visited

    visit :: [Int] -> IntSet.IntSet -> IntSet.IntSet
    visit as vs = IntSet.fromList $ [v - a | a <- as, v <- (IntSet.elems vs), v - a >= 0]

    candidates = a ++ b ++ c
    a = [1]
    b = filter (<= 10^5) (map (6^) [1..6])
    c = filter (<= 10^5) (map (9^) [1..9])


-- ABC088 D
{-
type Position = (Int, Int)
type Maze = Array Position Bool
solve :: Position -> Position -> Maze -> Int
solve start goal maze = numOfWhite - 1 - (bfs (Set.singleton start) (Set.singleton start) 0)
  where
    numOfWhite = length . filter id $ elems maze
    bfs :: Set.Set Position -> Set.Set Position -> Int -> Int
    bfs visiting visited n
      | visiting == Set.empty = numOfWhite
      | goal `Set.member` visiting = n
      | otherwise = bfs visit (Set.union visited visit) (n + 1)
        where
          adjacents = Set.fromList . join . map step $ Set.toList visiting
          visit = adjacents Set.\\ visited

    step :: Position -> [Position]
    step (x,y) = validate [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

    validate :: [Position] -> [Position]
    validate = filter (\p -> isInRange p (bounds maze) && maze!p)

    isInRange :: Position -> (Position, Position) -> Bool
    isInRange p (l, u) = lx <= px
                        && px <= ux
                        && ly <= py
                        && py <= uy
      where
        (px,py) = p
        (lx,ly) = l
        (ux,uy) = u

-}
