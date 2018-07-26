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

  let al = toAdjList gs
  putStr . unlines . map
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

getAdjs :: Int -> AdjList -> [Int]
getAdjs i = Map.findWithDefault [] i
