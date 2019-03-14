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

main :: IO ()
main = do
  n <- readLn :: IO Int
  pos <- fmap (map read . words) getLine :: IO [Int]
  ios <- fmap (map read . words) getLine :: IO [Int]
  solve pos ios

data Tree a = Leaf | Node a (Tree a) (Tree a) 

solve pos ios = postOrder $ reconstruct pos ios
  where
    reconstruct :: [Int] -> [Int] -> Tree Int
    reconstruct pos [] = Leaf
    reconstruct [] ios = Leaf
    reconstruct pos ios = Node p (reconstruct pos l) (reconstruct pos r)
      where
        (l, p, r) = fromJust . head . filter isJust $ map (flip splitIfEqual ios) pos

splitIfEqual :: (Eq a) => a -> [a] -> Maybe ([a], a, [a])
splitIfEqual k as = go k [] as
  where
    go _ _ [] = Nothing
    go k bs (a:as) = if k == a 
                     then Just (reverse bs, k, as)
                     else go k (a:bs) as

postOrder :: Tree Int -> IO ()
postOrder = putStrLn . unwords . map show . postOrderedList 
  where
    postOrderedList :: Tree Int -> [Int]
    postOrderedList Leaf = []
    postOrderedList (Node x left right) = (postOrderedList left) ++ (postOrderedList right) ++ [x]



