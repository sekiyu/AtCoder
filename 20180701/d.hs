
main = do
  n <- readLn :: IO Int
  as <- fmap (map read . words) getLine :: IO [Int]
  print $ solve as

solve (a:b:c:d:e) = innerSolve [a] [b] [c] [d] e
  where 
    ureshisa a b c d = max (map sum [a,b,c,d]) - minimum (map sum [a,b,c,d])
    innerSolve a b c d [] = ureshisa a b c d
    innerSolve a b c d (e:es) =
