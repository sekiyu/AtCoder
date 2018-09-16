import Data.Char

main :: IO ()
main = do
  s <- getLine
  print $ solve s

solve :: String -> Int
solve = patterns . map digitToInt

patterns :: [Int] -> Int
patterns [] = 0
patterns [a] = a
patterns (a:b:as) =
  a * 2^n
  + (patterns $ (10 * a + b):as)
  + (patterns $ b:as)
    where n = length as
