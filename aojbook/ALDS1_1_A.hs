import Data.List

main :: IO ()
main = do
  _ <- getLine
  as <- fmap (map read . words) getLine :: IO [Int]
  solve' as

solve' :: [Int] -> IO ()
solve' as
  | as == sort as = return ()
  | otherwise = do
      let bs = insertStep [] as
      putStrLn . unwords . map show $ bs
      solve' bs

insertStep :: [Int] -> [Int] -> [Int]
insertStep [] (a:as) = insertStep [a] as
insertStep bs [] = bs
insertStep bs (a:as) = insertStep (insert a bs) as
  where
    insert a [] = [a]
    insert a (c:cs)
      | a < c = a:c:cs
      | otherwise = c:(insert a cs)

solve :: [Int] -> [Int] -> IO ()
solve as []
  | as == sort as = return ()
solve (a:b:as) passed
  | a > b = do
      putStrLn . unwords . map show $ b:a:as
      solve d []
  | otherwise = do
      putStr $ show a ++ " "
      solve (b:as) (passed ++ [a])
    where d = passed ++ (b:a:as)
