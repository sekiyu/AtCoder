import Data.Functor
import Data.List

main :: IO ()
main = do
  _ <- getLine
  as <-  map read . words <$> getLine :: IO [Int]
  putStrLn . unwords . map show . sort $ as
  print $ solve as

solve :: (Ord a) => [a] -> Int
solve as = go 0 as
  where go n (a:[]) = n
        go n (a:as) = if m < a then go (n + 1) rep
                      else go n rep
          where m = minimum as
                --rep = reverse . replaceFirst m a . reverse $ as
                rep = replaceFirst m a as

replaceFirst :: (Eq a) => a -> a -> [a] -> [a]
replaceFirst _ _ [] = []
replaceFirst a b (l:ls)
  | a == l = b:ls
  | otherwise = l:replaceFirst a b ls
