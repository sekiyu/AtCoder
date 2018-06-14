import Data.Char

main :: IO()
main = do
  a <- getLine
  print . length $ filter (== '1') a
