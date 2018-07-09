import Data.List

main :: IO ()
main = do
  _ <- getLine
  as <- fmap (map read . words) getLine :: IO [Int]
  solve as 0

solve :: [Int] -> Int -> IO ()
solve as i
  | length as == 1 = putStrLn . show . head $ as
  | i == length as = return ()
  | otherwise = do
      let bs = insertStep as i
      putStrLn . unwords . map show $ bs
      solve bs (i + 1)

insertStep :: [Int] -> Int -> [Int]
insertStep as i = (insert a (take i as)) ++ (drop (i + 1) as)
  where
    a = as !! i
    insert a [] = [a]
    insert a (c:cs)
      | a < c = a:c:cs
      | otherwise = c:(insert a cs)


insert' :: [Int] -> [Int] -> [Int]
insert' [] (a:as) = insert' [a] as
insert' bs [] = bs
insert' bs (a:as) = insert' (insert a bs) as
  where
    insert a [] = [a]
    insert a (c:cs)
      | a < c = a:c:cs
      | otherwise = c:(insert a cs)
