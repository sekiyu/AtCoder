import Control.Monad

main :: IO()
main = do
  n <- readLn :: IO Int
  ts <- replicateM n $ map read . words <$> getLine :: IO [[Int]]
  print $ head $ solve ts

solve :: [[Int]] -> [(Int, Int, [Int])]
solve [] = []
solve (t:ts) = (id, k, cs):solve ts
  where (id:k:cs) = t

data Tree = EmptyTree | Node Int [Tree] deriving (Show)

type Parent = Int
type Left = Int
type Right = Int
data TreePLR = EmptyPLR | Node Int Parent Left Right deriving (Show)
