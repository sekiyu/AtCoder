main :: IO()
main = do
  na <- readLn
  nb <- readLn
  nc <- readLn
  x <- readLn
  print . length $ [1 | a <- [0..na], b <- [0..nb], c <- [0..nc], 500 * a + 100 * b + 50 * c == x]
