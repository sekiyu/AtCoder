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

import qualified Data.ByteString.Char8 as B

main :: IO()
main = do
  n <- readLn :: IO Int
  -- cs <- replicateM n $ fmap words getLine :: IO [[String]]
  -- solve cs Leaf
  cs <- replicateM n B.getLine :: IO [B.ByteString]
  solve' cs

solve' :: [B.ByteString] -> IO ()
solve' = foldM_ go Leaf
  where  
    go :: Tree Int -> B.ByteString -> IO (Tree Int)
    go !tree bs
      | c == bInsert = val `seq` return $ insertTree val tree
      | c == bPrint = do 
        printTree tree
        return tree
      | c == bFind = do
        putStrLn $ if (elemTree val tree) then "yes" else "no"
        return tree
      where
        (c:cs) = B.words bs
        val = fst . fromJust . B.readInt $ head cs :: Int

bInsert = B.pack "insert"
bPrint = B.pack "print"
bFind = B.pack "find"


printTree tree = do
  putStrLn . (' ':) . unwords . map show $ inorder tree
  putStrLn . (' ':) . unwords . map show $ preorder tree

data Tree a = Leaf | Node !a (Tree a) (Tree a) deriving (Show)

insertTree :: (Ord a) => a -> Tree a -> Tree a
insertTree !c Leaf = Node c Leaf Leaf
insertTree !c !(Node v left right)
  = if v < c
    then Node v left (insertTree c right)
    else Node v (insertTree c left) right

inorder :: Tree a -> [a]
inorder Leaf = []
inorder (Node v left right) = (inorder left) ++ [v] ++ (inorder right)

preorder :: Tree a -> [a]
preorder Leaf = []
preorder (Node v left right) = v:(preorder left) ++ (preorder right)

elemTree :: (Ord a) => a -> Tree a -> Bool
elemTree _ Leaf = False
elemTree key (Node v left right)
  | key == v = True
  | key < v = elemTree key left
  | key > v = elemTree key right

{-
solve :: [[String]] -> Tree Int -> IO()
solve [] _ = return ()
solve (c:cs) !t
  | command == "insert" = solve cs $ insertTree v t
  | command == "print" = do
      putStrLn . (' ':) . unwords . map show $ inorder t
      putStrLn . (' ':) . unwords . map show $ preorder t
      solve cs t
  | command == "find" = do
      putStrLn $ if (elemTree v t) then "yes" else "no"
      solve cs t
  where
    command = head c
    v = read $ c !! 1 :: Int
-}