-- skew heap
data Tree a = Null | Fork a (Tree a) (Tree a) deriving (Show)

isEmpty :: Tree a -> Bool
isEmpty Null = True
isEmpty _ = False

minElem :: Tree a -> a
minElem (Fork x _ _) = x

deleteMin :: (Ord a) => Tree a -> Tree a
deleteMin (Fork x a b) = merge a b

insert :: (Ord a) => a -> Tree a -> Tree a
insert x a = merge (singleton x) a

merge :: (Ord a) => Tree a -> Tree a -> Tree a
merge a Null = a
merge Null b = b
merge a b
  | minElem a <= minElem b = join a b
  | otherwise = join b a

join :: (Ord a) => Tree a -> Tree a -> Tree a
join (Fork x a b) c = Fork x b (merge a c)

singleton :: a -> Tree a
singleton a = Fork a Null Null

fromList :: (Ord a) => [a] -> Tree a
fromList = foldl (flip insert) Null
