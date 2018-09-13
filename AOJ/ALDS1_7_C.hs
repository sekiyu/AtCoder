import Control.Monad
import Data.List
import qualified Data.Map as Map
import Data.Maybe

main = do
  n <- readLn :: IO Int
  ts <- replicateM n $ fmap (map read . words) getLine :: IO [[Int]]
  let
    tree = makePLR ts
    root = rootId tree
    pre   = preorder root tree
    ino = inorder root tree
    post  = postorder root tree
  putStrLn "Preorder"
  putStr " "
  putStrLn . unwords $ map show pre
  putStrLn "Inorder"
  putStr " "
  putStrLn . unwords $ map show ino
  putStrLn "Postorder"
  putStr " "
  putStrLn . unwords $ map show post

preorder :: Int -> [(Int, Int, Int)] -> [Int]
preorder _ [] = []
preorder p tree = p:l ++ r
  where
    targetTree = filter (\(pid, _, _) -> pid == p) tree
    (_, lid, rid) = head targetTree
    l = if lid == -1 then [] else preorder lid tree
    r = if rid == -1 then [] else preorder rid tree

inorder :: Int -> [(Int, Int, Int)] -> [Int]
inorder _ [] = []
inorder p tree = l ++ [p] ++ r
  where
    targetTree = filter (\(pid, _, _) -> pid == p) tree
    (_, lid, rid) = head targetTree
    l = if lid == -1 then [] else inorder lid tree
    r = if rid == -1 then [] else inorder rid tree


postorder :: Int -> [(Int, Int, Int)] -> [Int]
postorder _ [] = []
postorder p tree = l ++ r ++ [p]
  where
    targetTree = filter (\(pid, _, _) -> pid == p) tree
    (_, lid, rid) = head targetTree
    l = if lid == -1 then [] else postorder lid tree
    r = if rid == -1 then [] else postorder rid tree


rootId :: [(Int, Int, Int)] -> Int
rootId tree = id
  where
    parents = clean $ makeParents tree
    depths = map (measureDepth parents) [0..(length tree - 1)]
    Just id = elemIndex 0 depths

clean = sort . filter (\(id, _) -> id /= -1)

makePLR :: [[Int]] -> [(Int, Int, Int)]
makePLR [] = []
makePLR (t:ts) = (id, l, r):(makePLR ts)
  where (id:l:r:_) = t

makeParents :: [(Int, Int, Int)] -> [(Int, Int)]
makeParents [] = []
makeParents (t:ts) = (l, p):(r, p):makeParents ts
  where (p, l, r) = t

measureDepth :: [(Int, Int)] -> Int -> Int
measureDepth parents id = if (length target) == 0 then 0 else (1 + measureDepth parents (snd $ head target))
  where
    target = filter (\(tid, _) -> tid == id) parents
