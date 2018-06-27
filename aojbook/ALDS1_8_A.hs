import Control.Monad

main :: IO()
main = do
  n <- readLn :: IO Int
  cs <- replicateM n $ fmap words getLine :: IO [[String]]
  solve cs EmptyTree

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

solve :: [[String]] -> Tree Int -> IO()
solve [] _ = return ()
solve (c:cs) t
  | command == "insert" = solve cs $ insertTree v t
  | command == "print" = do
      putStr " "
      putStrLn . unwords . map show $ inorder t
      putStr " "
      putStrLn . unwords . map show $ preorder t
      solve cs t
  where
    command = head c
    v = read $ c !! 1 :: Int

insertTree :: (Ord a) => a -> Tree a -> Tree a
insertTree c EmptyTree = Node c EmptyTree EmptyTree
insertTree c (Node v left right)
  | c >= v = Node v left (insertTree c right)
  | c < v = Node v (insertTree c left) right


inorder :: Tree a -> [a]
inorder EmptyTree = []
inorder (Node v left right) = (inorder left) ++ [v] ++ (inorder right)

preorder :: Tree a -> [a]
preorder EmptyTree = []
preorder (Node v left right) = v:(preorder left) ++ (preorder right)
