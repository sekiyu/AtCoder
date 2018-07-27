import Control.Monad
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (fromJust)

main :: IO ()
main = do
  (n:m:_) <-  fmap (map read . words) getLine :: IO [Int]
  gs <- replicateM m $ fmap (map read . words) getLine :: IO [[Int]]
  q <- readLn :: IO Int
  qs <- replicateM q $ fmap (map read . words) getLine :: IO [[Int]]
  --let al = toCompAdjList . toAdjList $ gs
  let al = toAdjList gs
  --print al
  putStr . unlines . map
    --(\(a:b:_) -> if b `elem` (getAdjs a al) then "yes" else "no") $ qs

    (\(a:b:_) -> if reachable' [] a [b] al then "yes" else "no") $ qs

    --(\(a:b:_) -> if reachable [] a b al then "yes" else "no") $ qs

type AdjList = Map.Map Int [Int]

-- first submission TLE@13
reachable :: [Int] -> Int -> Int -> AdjList -> Bool
reachable vs a b al
  | friendsOfB == [] = False
  | elem a friendsOfB = True
  | otherwise = any (flip (reachable (b:vs) a) al) friendsOfB
    where friendsOfB = filter (`notElem` vs) $ getAdjs b al

--多分anyの中で重複した探索をしてしまっている。
--訪問済みリストの他に、訪問予定リストも持つようにする

-- second submission TLE@18
reachable' :: [Int] -> Int -> [Int] -> AdjList -> Bool
reachable' _ _ [] _ = False
reachable' vs a (b:bs) al
  | elem a friendsOfB = True
  | otherwise = reachable' (b:vs) a (friendsOfB ++ bs) al
    where
      friendsOfB = filter (`notElem` vs) $ getAdjs b al

-- 事前に完全な隣接リスト(Adjacent List)を作っておく
-- あるいは、探索した結果で隣接リストをオンラインで更新していけば速くなる？

toAdjList :: [[Int]] -> AdjList
toAdjList qs = insertAdj qs Map.empty
  where
    insertAdj :: [[Int]] -> AdjList -> AdjList
    insertAdj [] al = al
    insertAdj (g:gs) al = insertAdj gs newal2
      where
        (x:y:_) = g
        newal1 = Map.insertWith (++) x [y] al
        newal2 = Map.insertWith (++) y [x] newal1

-- third submission WA@14
toCompAdjList :: AdjList -> AdjList
toCompAdjList al = foldr (\i acc -> complete i (getAdjs i acc) acc) al [0..Map.size al]
  where
    complete :: Int -> [Int] -> AdjList -> AdjList
    complete _ [] al = al
    complete i (s:ss) al = complete i (friends ++ ss) $ Map.adjust (friends ++) i al
      where
        friends = filter (`notElem` getAdjs i al) $ getAdjs s al


getAdjs :: Int -> AdjList -> [Int]
getAdjs i = Map.findWithDefault [] i
