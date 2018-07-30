import Control.Monad
import Data.Functor

main :: IO ()
main = solve Empty

solve :: MaxHeap Int -> IO ()
solve heap = do
  c <- words <$> getLine :: IO [String]
  case (head c) of
    "end" -> return ()
    "insert" -> solve $ insert (read . last $ c) heap
    "extract" -> let (v, t) = extract heap in do
      print v
      solve t

data MaxHeap a = Empty | Tree a (MaxHeap a) (MaxHeap a) deriving Show

insert :: (Ord a) => a -> MaxHeap a -> MaxHeap a
insert a Empty = Tree a Empty Empty
insert a (Tree x left right)
  | a > x     = Tree a right (insert x left)
  | otherwise = Tree x right (insert a left)

extract :: (Ord a, Bounded a) => MaxHeap a -> (a, MaxHeap a)
extract Empty = (minBound, Empty)
extract (Tree x left right) = (x, merge left right)

merge :: (Ord a, Bounded a) => MaxHeap a -> MaxHeap a -> MaxHeap a
merge t Empty = t
merge Empty t = t
merge (Tree x xl xr) (Tree y yl yr)
  | x < y     = Tree y (Tree x xl xr) (merge yl yr)
  | otherwise = Tree x (merge xl xr) (Tree y yl yr)
