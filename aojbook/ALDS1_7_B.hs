import Control.Monad
import Data.List
import qualified Data.Map as Map
import Data.Maybe

--main :: IO()
main = do
  n <- readLn :: IO Int
  ts <- replicateM n $ fmap (map read . words) getLine :: IO [[Int]]
  let
    tree = makePLR ts
    parents = clean $ makeParents tree
    siblings = clean $ makeSibling tree
    degrees = clean $ makeDegree tree
    depths = map (measureDepth parents) [0..(n-1)]
    heights = map (measureHeight tree) [0..(n-1)]
    --printTree :: Int -> IO()
    printTree id = putStrLn ("node " ++ (show id)
      ++ ": parent = " ++ p ++ ", sibling = " ++ s
      ++ ", degree = " ++ (show deg) ++ ", depth = " ++ (show dep)
      ++ ", height = " ++ (show h) ++ ", " ++ kind)
        where
          p = show $ search id parents
          s = show $ search id siblings
          Just deg = Map.lookup id $ Map.fromList degrees
          dep = depths !! id
          h = heights !! id
          kind = if dep == 0 then "root"
            else if deg == 0 then "leaf"
            else "internal node"
  mapM_ printTree [0..(n-1)]

clean = sort . filter (\(id, _) -> id /= -1)

search :: Int -> [(Int, Int)] -> Int
search id dic = if (isNothing target) then -1 else val
  where
    target = Map.lookup id $ Map.fromList dic
    Just val = target

makePLR :: [[Int]] -> [(Int, Int, Int)]
makePLR [] = []
makePLR (t:ts) = (id, l, r):(makePLR ts)
  where (id:l:r:_) = t

makeParents :: [(Int, Int, Int)] -> [(Int, Int)]
makeParents [] = []
makeParents (t:ts) = (l, p):(r, p):makeParents ts
  where (p, l, r) = t

makeSibling :: [(Int, Int, Int)] -> [(Int, Int)]
makeSibling [] = []
makeSibling (t:ts) = (l, r):(r, l):makeSibling ts
  where (p, l, r) = t

makeDegree :: [(Int, Int, Int)] -> [(Int, Int)]
makeDegree [] = []
makeDegree (t:ts) = (p, (count l) + (count r)):makeDegree ts
  where
    (p, l, r) = t
    count x = if x == -1 then 0 else 1

measureDepth :: [(Int, Int)] -> Int -> Int
measureDepth parents id = if (length target) == 0 then 0 else (1 + measureDepth parents (snd $ head target))
  where
    target = filter (\(tid, _) -> tid == id) parents

measureHeight :: [(Int, Int, Int)] -> Int -> Int
measureHeight [] _ = -1
measureHeight plr id = if (length target) == 0 then -1 else (1 + height)
  where
    target = filter (\(tid, _, _) -> tid == id) plr
    (p, l, r) = head target
    height = max (measureHeight plr l) (measureHeight plr r)
