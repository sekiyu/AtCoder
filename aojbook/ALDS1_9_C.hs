import Control.Monad
import Data.Functor

main :: IO ()
main = solve Empty

solve :: MaxHeap Int -> IO ()
solve heap = do
  c <- words <$> getLine :: IO [String]
  print c
  if (head c) == "end" then return ()
  else solve heap

data MaxHeap a = Empty | Tree a (MaxHeap a) (MaxHeap a) deriving Show

insert :: (Ord a) => a -> MaxHeap a -> MaxHeap a
insert a Empty = Tree a Empty Empty
insert a (Tree x left right)
  | a > x     = Tree a right (insert x left)
  | otherwise = Tree x right (insert a left)

head :: (Ord a) => MaxHeap a -> (a, MaxHeap a)
