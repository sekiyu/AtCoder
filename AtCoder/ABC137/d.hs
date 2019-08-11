{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Data.Maybe
import Debug.Trace
import Data.List
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
readInts = map readInt . B.words <$> B.getLine :: IO [Int]
readIntegers = map (fst . fromJust . B.readInteger) . B.words <$> B.getLine :: IO [Integer]
read2dInts = map (map (fst . fromJust . B.readInt) . B.words) . B.lines <$> B.getContents
readVInts = map (fst . fromJust . B.readInt) . B.lines <$> B.getContents
readVTuple = map ((\(a:b:_) -> (a,b)) . map (fst . fromJust . B.readInt) . B.words) . B.lines <$> B.getContents :: IO [(Int, Int)] 


main :: IO ()
main = do
  (n:m:_) <- map read . words <$> getLine :: IO [Int]
  abs <- replicateM n $ (\(a:b:_) -> (a,b)) . map read . words <$> getLine :: IO [(Int, Int)]
  -- abs <- map ((\(a:b:_) -> (a,b)) . map (fst . fromJust . B.readInt) . B.words) . B.lines <$> B.getContents
  print $ solve n m abs

solve n m abs = go (0, 0) initial wvs
  where
    initial = empty (\x y -> compare (snd y) (snd x))
    wvs = sortOn fst $ abs
    go (mm, mv) heap wvs
      | mm > m = mv
      | isEmpty heap = go (mm+1, mv) (foldr insertHeap heap next) newWvs
      | otherwise = go (mm+1, mv + maxb) (foldr insertHeap (removeRoot heap) next) newWvs
      where
        next = takeWhile (\x -> fst x == mm + 1) wvs
        newWvs = dropWhile (\x -> fst x == mm + 1) wvs
        maxb = snd $ root heap


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
