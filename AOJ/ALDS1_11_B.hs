import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- readLn
  gs <- replicateM n $ fmap (map read . words) getLine :: IO [[Int]]
  solve gs
  --print . reverse $ pathDFS (toAdjMatrix gs) [0] []

type Graph = [[Int]]

--solve :: Graph -> ([Int], [Int])
--solve gs = depthFirstSearch (toAdjMatrix gs) [] 0
solve gs = printResult . count . reverse $ pathDFS (toAdjMatrix gs) [0] []

printResult :: ([Int], [Int]) -> IO ()
printResult (fv, ds) = forM_ [0..(length fv - 1)] $ \i -> do
  putStrLn   . unwords . map show $ [i + 1, fv !! i, ds !! i]

pathDFS :: Graph -> [Int] -> [Int] -> [Int]
pathDFS gs [] vs = if unReached == []
                   then vs
                   else pathDFS gs [head unReached] (-1:vs)
  where
    n = length gs
    unReached = [i | i <- [0..(n - 1)], not(elem i vs)]

pathDFS gs (s:ss) vs = if unfinished == []
                        then pathDFS gs ss (s:vs)
                        else pathDFS gs (v:s:ss) (s:vs)
  where
    n = length gs
    edges = gs !! s
    unfinished = [i | i <- [0..(n - 1)], edges !! i == 1, not(elem i (s:vs))]
    v = head unfinished

count :: [Int] -> ([Int], [Int])
count path = (firstVisit path, doneSearch path)
  where
    firstVisit :: [Int] -> [Int]
    firstVisit path = map findFirst [0..m]
      where
        m = maximum path
        findFirst :: Int -> Int
        findFirst i = let Just j = elemIndex i path
                      in j + 1

    doneSearch :: [Int] -> [Int]
    doneSearch path = map (length path + 2 - ) . firstVisit . reverse $ path




depthFirstSearch :: Graph -> [Int] -> Int -> [Int]
--depthFirstSearch _ _ _ = []
depthFirstSearch gs vs i = i:join [depthFirstSearch gs (i:vs) j | j <- iunfinished] ++ [i]
  where
    edges = gs !! i
    n = length gs
    iunfinished = [j | j <- [0..(n - 1)], edges !! j == 1, not (elem j vs)]

toAdjMatrix :: Graph -> Graph
toAdjMatrix gs = scanColumn gs $ length gs
  where
    scanColumn :: Graph -> Int -> Graph
    scanColumn [] _ = []
    scanColumn (x:xs) n = (makeRow x n) : (scanColumn xs n)

    makeRow :: [Int] -> Int -> [Int]
    makeRow (u:k:vs) n = [if x `elem` vs then 1 else 0 | x <- [1..n]]
