import Control.Monad

main :: IO ()
main = do
  s <- getLine
  print . sum . map abc (candidates s)

candidates :: String -> [String]
candidates (s:ss)
  | s == '?' = 'A':(candidates ss)
    ++ 'B':(candidates ss) ++ 'C':(candidates ss)
  | otherwise = s:candidates ss

abc :: String -> Int
abc s = length [1 | i <- [0..(n - 3)], j <- [i..(n - 2)], k <- [j..(n - 1)],
  s !! i == 'A', s !! j == 'B', s !! k == 'C']
  where n = length s
