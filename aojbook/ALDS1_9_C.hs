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
insert a t = merge (Tree a Empty Empty) t

extract :: (Ord a) => MaxHeap a -> (a, MaxHeap a)
extract (Tree x left right) = (x, merge left right)

merge :: (Ord a) => MaxHeap a -> MaxHeap a -> MaxHeap a
merge t Empty = t
merge Empty t = t
merge a@(Tree x xl xr) b@(Tree y yl yr)
  | x < y     = Tree y yr (merge yl a)
  | otherwise = Tree x xr (merge xl b)
