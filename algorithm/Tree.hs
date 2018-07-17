module Tree where

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x orig@(Node y left right)
  | x == y = orig
  | x < y  = Node y (treeInsert x left) right
  | x > y  = Node y left (treeInsert x right)

treeFromList :: (Ord a) => [a] -> Tree a
treeFromList = foldr treeInsert EmptyTree

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node y left right)
  | x == y  = True
  | x < y   = treeElem x left
  | x > y   = treeElem x right

sampleTree :: Tree Int
sampleTree = treeFromList [2,8,4,1,6,7,3,9,5]
