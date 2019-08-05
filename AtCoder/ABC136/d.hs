import qualified Data.ByteString.Char8 as B
main = B.getLine >>= putStrLn . unwords . map show . solve . B.unpack

solve = toList . go 0 
  where
    go _ [] = []
    go i ('R':'L':ss) = (i, j):(go 0 $ dropWhile (=='L') ss)
      where j = length $ takeWhile (=='L') ss
    go i ('R':ss) = go (i + 1) ss
    
    toList [] = []
    toList ((i, j):ijs) = (replicate i 0) 
      ++ [x + j `mod` 2, x + i `mod` 2 ] 
      ++ (replicate j 0) 
      ++ (toList ijs)
        where
          x = 1 + i `div` 2 + j `div` 2

