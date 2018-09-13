{-# LANGUAGE BangPatterns #-}
import Control.Monad
-- import Control.DeepSeq
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
import Data.Functor

main :: IO()
main = do
  n <- readLn :: IO Int
  -- cs <- replicateM n $ fmap words getLine :: IO [[String]]
  -- solve cs EmptyTree

  -- cs <- map B.words . B.lines <$> B.getContents :: IO [[B.ByteString]]
  cs <- replicateM n $ B.words <$> B.getLine :: IO [[B.ByteString]]
  solveBs cs EmptyTree

solveBs :: [[B.ByteString]] -> Tree Int -> IO()
solveBs [] _ = return ()
solveBs (c:cs) t
  | B.unpack command == "insert" = solveBs cs $! insertTree v t
  | B.unpack command == "print" = do
      printTree t
      solveBs cs t
    where
      command = head c
      v = fst . fromJust . B.readInt $ c !! 1 :: Int

data Tree a = EmptyTree | Node !a !(Tree a) !(Tree a) deriving (Show)

solve :: [[String]] -> Tree Int -> IO()
solve [] _ = return ()
solve (c:cs) !t
  | command == "insert" = solve cs $! insertTree v t
  | command == "print" = do
      printTree t
      solve cs t
  where
    command = head c
    v = read $ c !! 1 :: Int

insertTree :: (Ord a) => a -> Tree a -> Tree a
insertTree c EmptyTree = Node c EmptyTree EmptyTree
insertTree c !(Node v !left !right)
  | c >= v = let !inserted = insertTree c right
             in Node v left inserted
  | c < v = let !inserted = (insertTree c left)
            in Node v inserted right

inorder :: Tree a -> [a]
inorder EmptyTree = []
inorder (Node v left right) = (inorder left) ++ [v] ++ (inorder right)

preorder :: Tree a -> [a]
preorder EmptyTree = []
preorder (Node v left right) = v:(preorder left) ++ (preorder right)

printTree :: Tree Int -> IO ()
printTree t = do
    putStr " "
    putStrLn . unwords . map show $ inorder t
    putStr " "
    putStrLn . unwords . map show $ preorder t
