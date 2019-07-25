{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Data.Maybe
import Debug.Trace
-- import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import Data.Functor
import Data.Array
-- import Data.Array.Unboxed
import Control.Monad.ST
import Data.Array.ST
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
readInt = fst . fromJust . B.readInt
readInts = map (fst . fromJust . B.readInt) . B.words <$> B.getLine :: IO [Int]
readIntegers = map (fst . fromJust . B.readInteger) . B.words <$> B.getLine :: IO [Integer]
read2dInts = map (map (fst . fromJust . B.readInt) . B.words) . B.lines <$> B.getContents

data Treap k a = Empty | Node k a (Treap k a) (Treap k a)
  deriving (Show)

key :: Treap k a -> k
key (Node k _ _ _) = k

priority :: Treap k a -> a
priority (Node _ a _ _) = a

insert :: (Ord k, Ord a) => k -> a -> Treap k a -> Treap k a
insert k p Empty = Node k p Empty Empty
insert k p org@(Node key pri left right)
  | k == key = org
  | k < key = rightRotate $ Node key pri (insert k p left) right
  | otherwise = leftRotate $ Node key pri left (insert k p right)

rightRotate :: (Ord a) => Treap k a -> Treap k a
rightRotate Empty = Empty
rightRotate org@(Node _ p left _) 
  | p < priority left = _rightRotate org
  | otherwise = org

_rightRotate (Node k p left right) = Node lk lp lleft (Node k p lright right)
  where
    (Node lk lp lleft lright) = left

leftRotate :: (Ord a) => Treap k a -> Treap k a
leftRotate Empty = Empty
leftRotate org@(Node _ p _ right) 
  | p < priority right = _leftRotate org
  | otherwise = org
  
_leftRotate (Node k p left right) = Node rk rp (Node k p left rleft) rright
  where
    (Node rk rp rleft rright) = right

delete :: (Ord k, Ord a) => k -> Treap k a -> Treap k a
delete _ Empty = Empty
delete k org@(Node key pri left right) 
  | k < key = Node key pri (delete k left) right
  | k > key = Node key pri left (delete k right)
  | otherwise = _delete k org

_delete :: (Ord k, Ord a) => k -> Treap k a -> Treap k a
_delete _ (Node _ _ Empty Empty) = Empty
_delete k org@(Node _ _ Empty _) = delete k $ _leftRotate org
_delete k org@(Node _ _ _ Empty) = delete k $ _rightRotate org
_delete k org@(Node key pri left right) = 
  if priority left > priority right
    then delete k $ _rightRotate org
    else delete k $ _leftRotate org

find :: (Ord k) => k -> Treap k a -> Bool
find k Empty = False
find k (Node key _ left right)
  | k == key = True
  | k < key = find k left
  | otherwise = find k right

printTreap :: (Show k) => Treap k a -> IO (Treap k a)
printTreap treap = do
  putStr " "
  putStrLn . unwords . map show $ inorder treap
  putStr " "
  putStrLn . unwords . map show $ preorder treap
  return treap

inorder :: Treap k a -> [k]
inorder Empty = []
inorder (Node k _ left right) = (inorder left) ++ [k] ++ (inorder right)

preorder :: Treap k a -> [k]
preorder Empty = []
preorder (Node k a left right) = k:(preorder left) ++ (preorder right)
  

main :: IO ()
main = do
  n <- readLn :: IO Int
  am <- B.lines <$> B.getContents :: IO [B.ByteString]
  solve am

solve = foldM_ f Empty 
  where
    f :: Treap Int Int -> B.ByteString -> IO (Treap Int Int)
    f treap b
      | order == bInsert =
        let [k, p] = bs
        in do
          return $ insert k p treap
      | order == bFind = 
        let k = head bs
        in do 
          putStrLn $ if find k treap then "yes" else "no"
          return treap
      | order == bDelete = return $ delete (head bs) treap
      | order == bPrint = printTreap treap
      | otherwise = return treap
        where
          order = head (B.words b) 
          bs = map readInt . tail $ B.words b


toInts = map readInt . B.words
bInsert = B.pack "insert"
bPrint = B.pack "print"
bFind = B.pack "find"
bDelete = B.pack "delete"
    
    