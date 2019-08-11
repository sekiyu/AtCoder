import Data.List (unfoldr, foldl')

-- skew heap
data Heap a = Heap (a -> a -> Ordering) (Tree a)
data Tree a = Null | Fork !a (Tree a) (Tree a) deriving (Show)

instance (Show a) => Show (Heap a) where
  show (Heap _ a) = show a

empty :: (a -> a -> Ordering) -> Heap a
empty pred = Heap pred Null

isEmpty :: Heap a -> Bool
isEmpty (Heap _ Null) = True
isEmpty _ = False

root :: Heap a -> a
root (Heap _ (Fork x _ _)) = x

rootNode :: Tree a -> a
rootNode (Fork x _ _) = x

removeRoot :: Heap a -> Heap a
removeRoot (Heap c (Fork _ a b)) = Heap c $ merge c a b

insertHeap :: a -> Heap a -> Heap a
insertHeap !x (Heap c t) = Heap c $ merge c (singleton x) t

merge :: (a -> a -> Ordering) -> Tree a -> Tree a -> Tree a
merge _ a Null = a
merge _ Null b = b
merge c a b
  | c (rootNode a) (rootNode b) == LT = joinHeap c a b
  | otherwise = joinHeap c b a

joinHeap :: (a -> a -> Ordering) -> Tree a -> Tree a -> Tree a
joinHeap c (Fork x l r) a = Fork x r (merge c l a)

singleton :: a -> Tree a
singleton a = Fork a Null Null

fromList :: (a -> a -> Ordering) -> [a] -> Heap a
fromList c = foldl' (flip insertHeap) (Heap c Null)


------- EXAMPLE 1 -----
-- *Main> fromList (compare) [4,1,1,5,7,4,8,6,0,2]
-- Fork 0 (Fork 1 (Fork 1 (Fork 7 Null Null) (Fork 8 Null Null)) (Fork 4 (Fork 4 Null (Fork 5 Null Null)) (Fork 6 Null Null))) (Fork 2 Null Null)
-- *Main> fromList (flip compare) [4,1,1,5,7,4,8,6,0,2]
-- Fork 8 (Fork 7 (Fork 4 Null Null) (Fork 5 (Fork 4 (Fork 1 Null Null) (Fork 1 Null Null)) (Fork 0 Null Null))) (Fork 6 Null (Fork 2 Null Null))

------- EXAMPLE 2 -----
-- *Main> let pq = fromList (flip compare) [4,1,1,5,7,4,8,6,0,2]
-- *Main> insert 10 pq 
-- Fork 10 Null (Fork 8 (Fork 7 (Fork 4 Null Null) (Fork 5 (Fork 4 (Fork 1 Null Null) (Fork 1 Null Null)) (Fork 0 Null Null))) (Fork 6 Null (Fork 2 Null Null)))
-- *Main> removeRoot pq
-- Fork 7 (Fork 5 (Fork 4 (Fork 1 Null Null) (Fork 1 Null Null)) (Fork 0 Null Null)) (Fork 6 (Fork 2 Null Null) (Fork 4 Null Null))




-- heapSort :: (Ord a) => [a] -> [a]
-- heapSort as = unfoldr f heap
--   where
--     heap = fromList as
--     f :: (Ord a) => Tree a -> Maybe (a, Tree a)
--     f h = if isEmpty h 
--           then Nothing
--           else Just (minElem h, deleteMin h)