import Control.Monad
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  (n:m:_) <-  fmap (map read . words) getLine :: IO [Int]
  gs <- replicateM m $ fmap (map read . words) getLine :: IO [[Int]]
  q <- readLn
  qs <- replicateM q $ fmap (map read . words) getLine :: IO [[Int]]
  let al = toAdjList gs
  putStr . unlines . map
    (\(a:b:_) -> if reachable [] a b al then "yes" else "no") $ qs

type AdjList = Map.Map Int [Int]

reachable :: [Int] -> Int -> Int -> AdjList -> Bool
reachable vs a b al
  | friendsOfB == [] = False
  | elem a friendsOfB = True
  | otherwise = any (flip (reachable (b:vs) a) al) friendsOfB
    where friendsOfB = filter (`notElem` vs) $ getAdjs b al

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
